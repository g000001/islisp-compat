(cl:in-package :islisp-compat.test)

(in-suite islisp-compat)


;;;
;;;  第１５章  Vectors
;;;

($ap 1 "Vectors")

;;;
;;; 関数 (BASIC-VECTOR-P obj) --> boolean
;;;
($ap 2 "basic-vector-p" P.94)
;;;
($argc basic-vector-p 1 0 0)
($predicate basic-vector-p $string $vector)

;;;
;;; 関数 (GENERAL-VECTOR-P obj) --> boolean
;;;
($ap 2 "general-vector-p" P.94)
($t (mapcar (lambda (x) (list (basic-vector-p x) (general-vector-p x)))
   '((a b c) "abc" #(a b c) #1a(a b c) #2a((a) (b) (c))))
    ((nil nil) (t nil) (t t) (t t) (nil nil))
    equal)
;;;
($argc general-vector-p 1 0 0)
($predicate general-vector-p $vector)

;;;
;;; 関数 (CREATE-VECTOR i [initial-element]) --> <general-vector>
;;;
($ap 2 "create-vector" P.94)
($t (create-vector 3 17) #(17 17 17) equal)
($t (create-vector 2 #\a) #(#\a #\a) equal)
;;;
($argc create-vector 1 1 0)
($type create-vector ($integer) :target)
;;;
($t (create-vector 0) #() equal)
($t (create-vector 0 'a) #() equal)
($error (create-vector -1) <domain-error>)
($error (create-vector -1234567890) <domain-error>)
($error (create-vector 1234567890) <storage-exhausted>)
($t (length (create-vector 1000 'a)) 1000 eql)

;;;
;;; 関数 (VECTOR obj*) --> <general-vector>
;;;
($ap 2 "vector" P.95)
($t (vector 'a 'b 'c) #(a b c) equal)
($t (vector) #() equal)
;;;
($argc vector 0 0 1)
($t (vector 1 2 3 4 5 6 7 8 9 10) #(1 2 3 4 5 6 7 8 9 10) equal)

;;; *EOF*
