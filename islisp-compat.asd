;;;; islisp-compat.asd -*- Mode: Lisp;-*- 

(cl:in-package :asdf)

(defsystem :islisp-compat
  :serial t
  :depends-on (:fiveam
               :named-readtables
               :fiveam)
  :components ((:file "package")
               (:file "readtable")
               (:file "islisp-compat")))

(defmethod perform ((o test-op) (c (eql (find-system :islisp-compat))))
  (load-system :islisp-compat)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :islisp-compat.internal :islisp-compat))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))

