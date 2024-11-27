;;;; pclog.asd -*- Mode: Lisp;-*- 

(cl:in-package :asdf)


(defsystem :pclog
  :serial t
  :components ((:file "package")
               (:file "var")
               (:file "trail")
               (:file "pclog")
               (:file "pclog-predicate")))


