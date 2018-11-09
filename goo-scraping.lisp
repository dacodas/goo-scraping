(defpackage :goo-scraping
  (:use #:cl)
  (:import-from :goo
                #:entry-number-from-response
                #:entry-number-parse-error))

(log:config :daily "/home/dacoda/quicklisp/local-projects/goo-scraping/progress.log" :backup nil :d)

(in-package :goo-scraping)

(defparameter *entries-directory* (make-pathname :directory (append
                                                     (pathname-directory (asdf:system-source-directory :goo-scraping))
                                                     '("dictionary-entries"))))

(defparameter *failures* 0)
(defparameter *user-agents*  (with-open-file (user-agents-stream "/home/dacoda/quicklisp/local-projects/goo-scraping/user-agents.txt")
                                 (loop for x = (read-line user-agents-stream nil nil)
                                    while x collect (list x (make-instance 'drakma:cookie-jar)))))
(defparameter *number-of-worker-threads* (floor (* 1.25 (length *user-agents*))))
(setf lparallel:*kernel* (lparallel:make-kernel *number-of-worker-threads*))

(defun grab-goo-pages (entries-array) 
  (let* ((number-of-pages (length entries-array))
         (futures (make-array number-of-pages)))
    (loop for i below number-of-pages
       do (let* ((user-agent-id (random (length *user-agents*)))
                 (user-agent (first (nth user-agent-id *user-agents*))))
            (setf (aref futures i)
                  (eval `(lparallel:future
                           (values-list (append (multiple-value-list
                                                 (drakma:http-request (format nil "https://dictionary.goo.ne.jp/jn/~A/meaning/m0u/" ,(aref entries-array i))
                                                                      :user-agent ,user-agent 
                                                                      :cookie-jar (second (nth ,user-agent-id *user-agents*))))
                                                (list ,user-agent))))))))
    futures))

(defun save-goo-pages (promises)
  (let ((output-directory *entries-directory*))
    (ensure-directories-exist output-directory)
    (loop for promise across promises
       collect (let* ((response (multiple-value-list (lparallel:force promise)))
                      (entry-number (handler-case (goo:entry-number-from-response response)
                                      (entry-number-parse-error ()
                                        (progn
                                          (log:debug "Got an error parsing! Assuming denied access... sleeping...~%")
                                          (incf *failures*)
                                          (sleep (random 1))
                                          (format nil "failure-~A" *failures*)))))
                      (output-file-name (merge-pathnames (make-pathname :name (format nil "~A" entry-number)
                                                                        :type "html")
                                                         output-directory)))
                 (log:debug "Saving entry number ~A~%" entry-number)
                 (with-open-file (output-file output-file-name
                                              :direction :output
                                              :if-exists :supersede
                                              :if-does-not-exist :create)
                   (format output-file "~A" (first response)))))))

(defun run ()
  (let* ((start-value 287870)
         (number-of-entries 295578)
         (number-per-chunk *number-of-worker-threads*)
         (number-of-chunks (/ (- number-of-entries start-value) number-per-chunk))
         (mean-sleep-time-between-requests 0)
         (mean-sleep-time-between-chunks 0.5)
         (mean-time-per-chunk (+ (* mean-sleep-time-between-requests number-per-chunk) mean-sleep-time-between-chunks))
         (total-time (* mean-time-per-chunk number-of-chunks)))

    (setf *failures* 0)
    (log:debug "Each chunk will take about ~s seconds~%" mean-time-per-chunk)
    (log:debug "Cumulatively all chunks will take about ~s seconds~%" total-time)
    
    (loop for chunk below number-of-chunks
       do (let* ((sleep-time (random (* 2 mean-sleep-time-between-chunks)))
                 (current-start-value (+ (* chunk number-per-chunk) start-value))) 
            
            (log:debug "Sleeping for ~a seconds~%" sleep-time)
            (sleep sleep-time)
            (log:debug "Processing chunk ~a starting with value ~a getting ~a pages~%"
                       chunk current-start-value number-per-chunk)

            (let ((page-promises (grab-goo-pages current-start-value number-per-chunk)))
              (save-goo-pages page-promises))))))

(defun find-missing-entries ()
  (defparameter *missing-entries*
    (let* ((number-of-entries (parse-integer
                               (with-output-to-string (*standard-output*)
                                 (sb-ext:run-program "/usr/bin/bash"
                                                     '("-c" "ls /home/dacoda/quicklisp/local-projects/goo-scraping/dictionary-entries | grep -v failure | wc -l")
                                                     :output *standard-output*
                                                     :if-output-exists :supersede))))
           (entries (make-array number-of-entries))
           (entries-list (with-output-to-string (*standard-output*)
                           (sb-ext:run-program "/usr/bin/bash"
                                               '("-c" "ls /home/dacoda/quicklisp/local-projects/goo-scraping/dictionary-entries | grep -v failure | sort -n")
                                               :output *standard-output*
                                               :if-output-exists :supersede)))
           (index -1))
      (cl-ppcre:do-register-groups (number) ("\([0-9]+\)" entries-list) 
        (setf (aref entries (incf index)) (parse-integer number)))
      (defparameter *entries* entries)
      (let* ((counter 2)
             (entries-index 0)
             (missing-entries-index -1)
             (highest-entry (aref entries (1- (length entries))))
             (number-missing-entries (-  highest-entry number-of-entries))
             (missing-entries (make-array number-missing-entries)))
        (loop while (< entries-index (length entries))
           do (progn
                (if (not (eq counter (aref entries entries-index)))
                    (progn (setf (aref missing-entries (incf missing-entries-index))
                                 counter))
                    (incf entries-index))
                (incf counter)))
        (format t "Done~%")
        missing-entries))))

(defun grab-missing-pages ()
  (find-missing-entries)
  (let* ((entries-left (length *missing-entries*))
         (number-per-chunk 5)
         (number-of-chunks (floor entries-left number-per-chunk)))
    (dotimes (chunk number-of-chunks)
      (sleep (random (* 2 0.5)))
      (save-goo-pages
       (grab-goo-pages
        (make-array number-per-chunk :displaced-to *missing-entries* :displaced-index-offset (* number-per-chunk chunk)))))))

(defun simplify-entry (entry-number)
  (let* ((simplified-entries-directory (make-pathname :directory (append
                                                                  (pathname-directory (asdf:system-source-directory :goo-scraping))
                                                                  '("simplified-dictionary-entries"))))
         (simplified-entry (merge-pathnames (make-pathname :name (format nil "~A" entry-number) :type "html")
                                            simplified-entries-directory))
         (original-entry (merge-pathnames (make-pathname :directory (pathname-directory *entries-directory*))
                                          simplified-entry)))
    (ensure-directories-exist simplified-entries-directory)
    (with-open-file (output-file simplified-entry
                                 :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
      (let ((document (lquery:$1 (initialize original-entry))))
        (format output-file "~A~%~A"
                (lquery:$1 document "div.basic_title h1" (text-without-comments))
                (lquery:$1 document "div.meaning_area" (serialize)))))))

(let* ((start 2)
       (end 259849)
       (number-per-chunk 500)
       (number-of-chunks (ceiling (1+ (- end start)) number-per-chunk))
       (chunk (make-array number-per-chunk)))
  (dotimes (chunk-number number-of-chunks)
    (dotimes (current-iteration number-per-chunk)
      (let ((current-entry (+ (* number-per-chunk chunk-number) current-iteration start)))
        (setf (aref chunk current-iteration)
              (eval `(lparallel:future (simplify-entry ,current-entry))))))
    (loop for entry across chunk do (lparallel:force entry))))

