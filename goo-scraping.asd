;;;; goo-scraping.asd

(asdf:defsystem goo-scraping
  :description "Scraping the goo dictionary"
  :serial t
  :components ((:file "goo-scraping"))
  :depends-on (:drakma :lparallel :goo))
