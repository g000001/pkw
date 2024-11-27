;;;; PKW.asd -*- Mode: Lisp;-*- 

(cl:in-package :asdf)


(defsystem :pkw
  :serial T
  :depends-on (:lisa
               :pclog
               ;;:fiveam
               )
  :components ((:file "package")
               (:file "pkw")
               #|(:file "test")|#))


(defmethod perform ((o test-op) (c (eql (find-system :pkw))))
  (let ((*package*
         (find-package
          "https://github.com/g000001/pkw#internals")))
    (eval
     (read-from-string
      "
      (or (let ((result (run 'PKW)))
            (explain! result)
            (results-status result))
          (error \"test-op failed\") )"))))


;;; *EOF*
