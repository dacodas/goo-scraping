(defpackage :goo-scraping
  (:use #:cl)
  (:import-from :goo
                #:entry-number-from-response
                #:entry-number-parse-error))

(log:config :daily "/home/dacoda/quicklisp/local-projects/goo-scraping/progress.log" :backup nil :d)

(in-package :goo-scraping)

(defparameter *failures* 0)
(defparameter *user-agents*  (with-open-file (user-agents-stream "/home/dacoda/quicklisp/local-projects/goo-scraping/user-agents.txt")
                                 (loop for x = (read-line user-agents-stream nil nil)
                                    while x collect (list x (make-instance 'drakma:cookie-jar)))))
(defparameter *number-of-worker-threads* (floor (* 1.25 (length *user-agents*))))
(setf lparallel:*kernel* (lparallel:make-kernel *number-of-worker-threads*))

(defun grab-goo-pages (start number-of-pages)
  (let ((futures (make-array number-of-pages)))
    (loop for i below number-of-pages
       do (let* ((user-agent-id (random (length *user-agents*)))
                 (user-agent (first (nth user-agent-id *user-agents*))))
            (setf (aref futures i)
                  (eval `(lparallel:future
                           (values-list (append (multiple-value-list
                                                 (drakma:http-request (format nil "https://dictionary.goo.ne.jp/jn/~A/meaning/m0u/" (+ ,start ,i))
                                                                      :user-agent ,user-agent 
                                                                      :cookie-jar (second (nth ,user-agent-id *user-agents*))))
                                                (list ,user-agent))))))))
    futures))

(defun save-goo-pages (promises)
  (let ((output-directory (make-pathname :directory (append
                                                     (pathname-directory (asdf:system-source-directory :goo-scraping))
                                                     '("dictionary-entries")))))
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

#+nil(let ((entries-left (length *missing-entries*))
      (number-per-chunk 100)
      (number-of-chunks (floor entries-left number-per-chunk)))

  (dotimes (chunk number-of-chunks)

    (let ((promises (make-array number-per-chunk))))))

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
