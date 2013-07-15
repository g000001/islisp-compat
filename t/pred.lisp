(cl:in-package :islisp-compat.test)

(in-suite islisp-compat)


;;;
;;;  第５章  Predicates
;;;

($ap 1 "Predicates")

;;;
;;; 関数 EQ
;;;
($ap 2 "eq" P.26)
($t (eq () ()) t)
($t (eq '() '()) t)
($t (eq 'a 'a) t)
($t (eq 'a 'A) t)
($t (eq 'a 'b) nil)
($t (eq 'f 'nil) nil)
;;((eq 2 2) t)						; nil or t (IDEF)
($t (eq 2 2.0) nil)
;;((eq 100000000 100000000) nil)			; nil or t (IDEF)
;;((eq 10.00000 10.0) nil)				; nil or t (IDEF)
($t (eq (cons 1 2) (cons 1 2)) nil)
($t (let ((x '(a))) (eq x x)) t)
;;((eq '(a) '(a)) nil)					; nil or t (IDEF)
;;((let ((x '(b)) (y '(a b))) (eq x (cdr y))) nil)	; nil or t (IDEF)
;;((eq '(b) (cdr '(a b))) nil)				; nil or t (IDEF)
($t (let ((p (lambda (x) x))) (eq p p)) t)
($t (let ((x "a")) (eq x x)) t)
;;((eq "a" "a") nil)					; nil or t (IDEF)
($t (let ((x "")) (eq x x)) t)
;;((eq "" "") nil)					; nil or t (IDEF)
($t (eq #\a #\A) nil)
;;((eq #\a #\a) t)					; nil or t (IDEF)
;;((eq #\space #\Space) t)				; nil or t (IDEF)
;;((eq #\space #\space) t)				; nil or t (IDEF)
;;;
($argc eq 2 0 0)

;;;
;;; 関数 (EQL obj1 obj2) --> boolean
;;;
($ap 2 "eql" P.26)
($t (eql () ()) t)
($t (eql '() '()) t)
($t (eql 'a 'a) t)
($t (eql 'a 'A) t)
($t (eql 'a 'b) nil)
($t (eql 'f 'nil) nil)
($t (eql 2 2) t)
($t (eql 2 2.0) nil)
($t (eql 100000000 100000000) t)
($t (eql 10.00000 10.0) t)
($t (eql (cons 1 2) (cons 1 2)) nil)
($t (let ((x '(a))) (eql x x)) t)
;;((eql '(a) '(a)) nil)					; nil or t (IDEF)
;;((let ((x '(b)) (y '(a b))) (eql x (cdr y))) nil)	; nil or t (IDEF)
;;((eql '(b) (cdr '(a b))) nil)				; nil or t (IDEF)
($t (let ((p (lambda (x) x))) (eql p p)) t)
($t (let ((x "a")) (eql x x)) t)
;;((eql "a" "a") nil)					; nil or t (IDEF)
($t (let ((x "")) (eql x x)) t)
;;((eql "" "") nil)					; nil or t (IDEF)
($t (eql #\a #\A) nil)
($t (eql #\a #\a) t)
($t (eql #\space #\Space) t)
($t (eql #\space #\space) t)
;;;
($argc eql 2 0 0)
;;;
($t (eql *most-negative-float* *most-negative-float*) t)
($t (eql *most-negative-float* *most-positive-float*) nil)

;;;
;;; 関数 (EQUAL obj1 obj2) --> boolean
;;;
($ap 2 "equal" P.28)
($t (equal 'a 'a) t)
($t (equal 2 2) t)
($t (equal 2 2.0) nil)
($t (equal '(a) '(a)) t)
($t (equal '(a (b) c) '(a (b) c)) t)
($t (equal (cons 1 2) (cons 1 2)) t)
($t (equal '(a) (list 'a)) t)
($t (equal "abc" "abc") t)
($t (equal (vector 'a) (vector 'a)) t)
($t (equal #(a b) #(a b)) t)
($t (equal #(a b) #(a c)) nil)
($t (equal "a" "A") nil)
;;;
($argc equal 2 0 0)
;;;
($t (equal 2.0 2.0) t)
($t (equal *most-negative-float* *most-negative-float*) t)
($t (equal *most-negative-float* *most-positive-float*) nil)
($t (equal "" "") t)
($t (equal (create-string 1000 #\a) (create-string 1000 #\a)) t)
($t (equal (create-string 1000 #\a) (create-string 1000 #\b)) nil)
($t (equal #() #()) t)
($t (equal #(a #(b) c) #(a #(b) c)) t)
($t (equal #(a #(b) c) #(a #(d) c)) nil)
($t (equal (create-vector 1000 'a) (create-vector 1000 'a)) t)
($t (equal (create-vector 1000 'a) (create-vector 1000 'b)) nil)
($t (equal (create-array () 'a) (create-array () 'a)) t)
($t (equal (create-array () 'a) (create-array () 'b)) nil)
($t (equal (create-array '(1 2 3) 'a) (create-array '(1 2 3) 'a)) t)
($t (equal (create-array '(1 2 3) 'a) (create-array '(1 2 3) 'b)) nil)

;;;
;;; 関数 (NOT obj) --> boolean
;;;
($ap 2 "not" P.29)
($t (not t) nil)
($t (not '()) t)
($t (not 'nil) t)
($t (not nil) t)
($t (not 3) nil)
($t (not (list)) t)
($t (not (list 3)) nil)
;;;
($argc not 1 0 0)
($predicate not $null)

;;;
;;; 特殊形式 (AND form*) --> <object>
;;;
($ap 2 "and" P.29)
($t (and (= 2 2) (> 2 1)) t)
($t (and (= 2 2) (< 2 1)) nil)
($t (and (eql 'a 'a) (not (> 1 2))) t)
($t (let ((x 'a)) (and x (setq x 'b))) b)
($t (let ((x nil)) (and x (setq x 'b))) nil)
($t (let ((time 10))
   (if (and (< time 24) (> time 12))
       (- time 12) time))
 10
 eql)
($t (let ((time 18))
   (if (and (< time 24) (> time 12))
       (- time 12) time))
 6
 eql)
;;;
($argc and 0 0 1)
;;;
($t (and) t)
($t (and (cons 1 2)) (1 . 2) equal)
($t (and (cons 1 2) nil (cons 3 4)) nil)
($t (and (cons 1 2) (cons 3 4) nil) nil)
($t (and (cons 1 2) (cons 3 4) (cons 5 6)) (5 . 6) equal)

;;;
;;; 特殊形式 (OR form*) --> <object>
;;;
($ap 2 "or" P.30)
($t (or (= 2 2) (> 2 1)) t)
($t (or (= 2 2) (< 2 1)) t)
($t (let ((x 'a)) (or x (setq x 'b))) a)
($t (let ((x nil)) (or x (setq x 'b))) b)
;;;
($argc or 0 0 1)
;;;
($t (or) nil)
($t (or (cons 1 2)) (1 . 2) equal)
($t (or nil (cons 1 2) (cons 3 4)) (1 . 2) equal)
($t (or (cons 1 2) (cons 3 4) (cons 5 6)) (1 . 2) equal)
($t (or nil nil nil nil nil) nil)

;;; *EOF*
