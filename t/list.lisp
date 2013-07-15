(cl:in-package :islisp-compat.test)

(in-suite islisp-compat)


;;;
;;;  第１３章  List class
;;;
($ap 1 "List class")

;;;
;;; 関数 (CONSP obj) --> boolean
;;;
($ap 2 "consp" P.83)
($t (consp '(a . b)) t)
($t (consp '(a b c)) t)
($t (consp '()) nil)
($t (consp #(a b)) nil)
;;;
($argc consp 1 0 0)
($predicate consp $cons)
;;;
($t (consp (create-list 1000 'a)) t)

;;;
;;; 関数 (CONS obj1 obj2) --> <cons>
;;;
($ap 2 "cons" P.84)
($t (cons 'a '()) (a) equal)
($t (cons '(a) '(b c d)) ((a) b c d) equal)
($t (cons "a" '(b c)) ("a" b c) equal)
($t (cons 'a 3) (a . 3) equal)
($t (cons '(a b) 'c) ((a b) . c) equal)
;;;
($argc cons 2 0 0)
;;;
($t (length (cons (create-list 1000 'a) (create-list 1000 'b))) 1001 eql)

;;;
;;; 関数 (CAR cons) --> <object>
;;;
($ap 2 "car" P.84)
($error (car '()) <domain-error>)

($t (car '(a b c)) a)
($t (car '((a) b c d)) (a) equal)
($t (car '(1 . 2)) 1 eql)
;;;
($argc car 1 0 0)
($type car ($cons) :target)
;;;
($t (car (create-list 1000 'a)) a)

;;;
;;; 関数 (CDR cons) --> <object>
;;;
($ap 2 "cdr" P.84)
($error (cdr '()) <domain-error>)
($t (cdr '((a) b c d)) (b c d) equal)
($t (cdr '(1 . 2)) 2 eql)
;;;
($argc cdr 1 0 0)
($type cdr ($cons) :target)
;;;
($t (length (cdr (create-list 1000 'a))) 999 eql)

;;;
;;; 関数 (SET-CAR obj cons) --> <object>
;;;
($ap 2 "set-car" P.85)
($t (let ((x (list 'apple 'orange)))
   (list x (car x) (setf (car x) 'banana) x (car x)))
 ((banana orange) apple banana (banana orange) banana)
 equal)
($t (let ((x (list 'apple 'orange)))
   (list x (car x) (set-car 'banana x) x (car x)))
 ((banana orange) apple banana (banana orange) banana)
 equal)
;;;
($argc set-car 2 0 0)
($type set-car ($cons) 1 :target)
;;;
($t (let ((x (create-list 1000 'a))) (list (setf (car x) 'b) (car x))) 
 (b b) equal)
($t (let ((x (create-list 1000 'a))) (list (set-car 'b x) (car x))) 
 (b b) equal)

;;;
;;; 関数 (SET-CDR obj cons) --> <object>
;;;
($ap 2 "set-cdr" P.85)
($t (let ((x (list 'apple 'orange)))
   (list x (cdr x) (setf (cdr x) 'banana) x (cdr x)))
 ((apple . banana) (orange) banana (apple . banana) banana)
 equal)
($t (let ((x (list 'apple 'orange)))
   (list x (cdr x) (set-cdr 'banana x) x (cdr x)))
 ((apple . banana) (orange) banana (apple . banana) banana)
 equal)
;;;
($argc set-cdr 2 0 0)
($type set-cdr ($cons) 1 :target)
;;;
($t (let ((x (create-list 1000 'a))) (list (setf (cdr x) 'b) x)) 
 (b (a . b)) equal)
($t (let ((x (create-list 1000 'a))) (list (set-cdr 'b x) x)) 
 (b (a . b)) equal)

;;;
;;; 関数 (NULL obj) --> boolean
;;;
($ap 2 "null" P.85)
($t (null '(a b c)) nil)
($t (null '()) t)
($t (null (list)) t)
;;;
($argc null 1 0 0)
($predicate null $null)
;;;
($t (null (create-list 1000 'a)) nil)

;;;
;;; 関数 (LISTP obj) --> boolean
;;;
($ap 2 "listp" P.86)
($t (listp '(a b c)) t)
($t (listp '()) t)
($t (listp '(a . b)) t)
($t (let ((x (list 'a))) (setf (cdr x) x) (listp x)) t)
($t (listp "abc") nil)
($t (listp #(1 2)) nil)
($t (listp 'jerome) nil)
;;;
($argc listp 1 0 0)
($predicate listp $cons $null)
;;;
($t (listp (create-list 1000 'a)) t)

;;;
;;; 関数 (CREATE-LIST i [initial-element]) --> <list>
;;;
($ap 2 "create-list" P.86)
($t (create-list 3 17) (17 17 17) equal)
($t (create-list 2 #\a) (#\a #\a) equal)
;;;
($argc create-list 1 1 0)
($type create-list ($integer) :target)
;;;
($t (create-list 0) ())
($t (create-list 0 1) ())
($error (create-list -1) <domain-error>)
($error (create-list 1234567890) <storage-exhausted>)
($error (create-list 1000000000000000) <storage-exhausted>)
($t (length (create-list 1000)) 1000 eql)
($t (length (create-list 1000 'a)) 1000 eql)

;;;
;;; 関数 (LIST obj*) --> <list>
;;;
($ap 2 "list" P.87)
($t (list 'a (+ 3 4) 'c) (a 7 c) equal)
($t (list) nil)
;;;
($argc list 0 0 1)
;;;
($t (list 1 2 3 4 5 6 7 8 9 10) (1 2 3 4 5 6 7 8 9 10) equal)

;;;
;;; 関数 (REVERSE list) --> <list>
;;;
($ap 2 "reverse" P.87)
($t (reverse '(a b c d e)) (e d c b a) equal)
($t (reverse '(a)) (a) equal)
($t (reverse '()) ())
;;;
($argc reverse 1 0 0)
($type reverse ($cons $null) :target)
;;;
($t (length (reverse (create-list 1000 'a))) 1000 eql)

;;;
;;; 関数 (NREVERSE list) --> <list>
;;;
($ap 2 "nreverse" P.87)
;;((let* ((x (list 'a 'b)) (y (nreverse x))) (equal x y)) nil)	; IDEF
;;;
($argc nreverse 1 0 0)
($type nreverse ($cons $null) :target)
;;;
($t (length (nreverse (create-list 1000 'a))) 1000 eql)

;;;
;;; 関数 (APPEND list*) --> <list>
;;;
($ap 2 "append" P.87)
($t (append '(a b c) '(d e f)) (a b c d e f) equal)
;;;
($argc append 0 0 1)
($type append ($cons $null) :target)
($type append ($cons $null) (quote (a b c)) :target)
($type append ($cons $null) (quote (a b c)) (quote (d e f)) :target)
;;; 0 引数
($t (append) ())
;;; 1 引数
($t (append ()) ())
($t (append '(a b c)) (a b c) equal)
($t (length (append (create-list 1000 'a))) 1000 eql)
;;; 2 引数
($t (append () ()) ())
($t (append () '(a b c)) (a b c) equal)
($t (append '(a b c) ()) (a b c) equal)
($t (length (append (create-list 1000 'a) (create-list 1000 'b))) 2000 eql)
;;; 3 引数
($t (append () () ()) ())
($t (append () () '(a b c)) (a b c) equal)
($t (append () '(a b c) ()) (a b c) equal)
($t (append '(a b c) () ()) (a b c) equal)
($t (append '(a b c) '(d e f) ()) (a b c d e f) equal)
($t (append '(a b c) () '(d e f)) (a b c d e f) equal)
($t (append () '(a b c) '(d e f)) (a b c d e f) equal)
($t (length 
  (append (create-list 1000 'a) (create-list 1000 'b) (create-list 1000 'c)))
 3000 eql)
;;; 最後のリストだけ共有する
($t (let* ((x (list 'a 'b 'c))
	(y (append x)))
   (eq y x))
 t)
($t (let* ((x (list 'a 'b 'c))
	(y (append '(1 2) x)))
   (eq (cdr (cdr y)) x))
 t)
($t (let* ((x (list 'a 'b 'c))
	(y (append '(1 2) '(3 4) x)))
   (eq (cdr (cdr (cdr (cdr y)))) x))
 t)
($t (let* ((x (list 'a 'b 'c))
	(y (append '(1 2) x '(3 4))))
   (eq (cdr (cdr y)) x))
  nil)

;;;
;;; 関数 (MEMBER obj list) --> <list>
;;;
($ap 2 "member" P.88)
($t (member 'c '(a b c d e f)) (c d e f) equal)
($t (member 'g '(a b c d e f)) nil)
($t (member 'c '(a b c a b c)) (c a b c) equal)
;;;
($argc member 2 0 0)
($type member ($cons $null) (quote a) :target)
;;;
($t (member #\b '(#\a #\b #\c)) (#\b #\c) equal)
($t (member 2 '(1 2 3)) (2 3) equal)
($t (member -2 '(1 -2 3)) (-2 3) equal)
($t (member 2.0 '(1 2.0 3)) (2.0 3) equal)
($t (member -2.0 '(1 -2.0 3)) (-2.0 3) equal)
($t (member 1234567890 '(1 1234567890 3)) (1234567890 3) equal)
($t (member -1234567890 '(1 -1234567890 3)) (-1234567890 3) equal)
($t (member 'a ()) nil)
($t (length (member 'a (create-list 1000 'a))) 1000 eql)
($t (member 'b (create-list 1000 'a)) nil)

;;;
;;; 関数 (MAPCAR function list+) --> <list>
;;;
($ap 2 "mapcar" P.88)
($t (mapcar #'car '((1 a) (2 b) (3 c))) (1 2 3) equal)
($t (mapcar #'abs '(3 -4 2 -5 -6)) (3 4 2 5 6) equal)
($t (mapcar #'cons '(a b c) '(1 2 3)) ((a . 1) (b . 2) (c . 3)) equal)
;;;
($argc mapcar 2 0 1)
($type mapcar ($function $generic) :target nil)
($type mapcar ($cons $null) (function list) :target)
($type mapcar ($cons $null) (function list) (quote (a b)) :target)
;;; 1 引数関数
($t (mapcar #'car ()) ())
($t (mapcar (lambda (x) x (car 1)) ()) ())
($t (mapcar (lambda (x) (+ x 1)) '(1 2 3 4 5)) (2 3 4 5 6) equal)
;;; 2 引数関数
($t (mapcar #'cons () ()) ())
($t (mapcar #'cons () '(a b c)) ())
($t (mapcar #'cons '(a b c) ()) ())
($t (mapcar (lambda (x y) (+ x y)) '(1 2 3 4 5) '(6 7 8 9 10)) 
 (7 9 11 13 15) equal)
;;; rest 引数関数
($t (mapcar #'list () () () () ()) ())
($t (mapcar #'list '(a b c) '(d e f) '(g h i) '(j k l) '(m n o))
 ((a d g j m) (b e h k n) (c f i l o))
 equal)
($t (mapcar #'list '(a b c) '(d e f) '(g) '(j k l) '(m n o))
 ((a d g j m))
 equal)
($t (mapcar (lambda (&rest x) x) '(1 2 3) '(4 5 6) '(7 8 9)) 
 ((1 4 7) (2 5 8) (3 6 9)) equal)

;;;
;;; 関数 (MAPC function list+) --> <list>
;;;
($ap 2 "mapc" P.88)
($t (let ((x 0)) (mapc (lambda (v) (setq x (+ x v))) '(3 5)) x) 8 eql)
;;;
($argc mapc 2 0 1)
($type mapc ($function $generic) :target nil)
($type mapc ($cons $null) (function list) :target)
($type mapc ($cons $null) (function list) (quote (a b)) :target)
;;; 1 引数関数
($t (mapc #'car ()) ())
($t (mapc (lambda (x) x (car 1)) ()) ())
($t (let ((x 0)) (list (mapc (lambda (v) (setq x (+ x v))) '(3 5)) x)) 
 ((3 5) 8) equal)
;;; 2 引数関数
($t (mapc #'cons () ()) ())
($t (mapc #'cons () '(a b c)) ())
($t (mapc #'cons '(a b c) ()) (a b c) equal)
($t (let ((ret 0)) 
   (list (mapc (lambda (x y) (setq ret (+ ret x y))) '(1 2 3) '(4 5 6)) ret))
 ((1 2 3) 21)
 equal)
;;; rest 引数関数
($t (mapc #'list () () () () ()) ())
($t (mapc #'list '(a b c) '(d e f) '(g h i) '(j k l) '(m n o)) (a b c) equal)
($t (mapc #'list '(a b c) '(d e f) '(g) '(j k l) '(m n o)) (a b c) equal)
($t (let ((ret ()))
   (list (mapc (lambda (&rest x) (setq ret (cons x ret))) '(1 2 3) '(4 5 6) '(7 8 9))
         ret))
 ((1 2 3) ((3 6 9) (2 5 8) (1 4 7)))
 equal)

;;;
;;; 関数 (MAPCAN function list+) --> <list>
;;;
($ap 2 "mapcan" P.88)
($t (mapcan (lambda (x) (if (> x 0) (list x))) '(-3 4 0 5 -2 7)) (4 5 7) equal)
;;;
($argc mapcan 2 0 1)
($type mapcan ($function $generic) :target nil)
($type mapcan ($cons $null) (function list) :target)
($type mapcan ($cons $null) (function list) (quote (a b)) :target)
;;; 1 引数関数
($t (mapcan #'car ()) ())
($t (mapcan (lambda (x) x (car 1)) ()) ())
($t (mapcan (lambda (x) (list (+ x 1))) '(1 2 3 4 5)) (2 3 4 5 6) equal)
;;; 2 引数関数
($t (mapcan #'cons () ()) ())
($t (mapcan #'cons () '(a b c)) ())
($t (mapcan #'cons '(a b c) ()) ())
($t (mapcan (lambda (x y) (list (+ x y))) '(1 2 3 4 5) '(6 7 8 9 10)) (7 9 11 13 15) equal)
;;; rest 引数関数
($t (mapcan #'list () () () () ()) ())
($t (mapcan #'list '(a b c) '(d e f) '(g h i) '(j k l) '(m n o))
 (a d g j m b e h k n c f i l o)
 equal)
($t (mapcan #'list '(a b c) '(d e f) '(g) '(j k l) '(m n o))
 (a d g j m)
 equal)
($t (mapcan (lambda (&rest x) x) '(1 2 3) '(4 5 6) '(7 8 9)) (1 4 7 2 5 8 3 6 9) equal)

;;;
;;; 関数 (MAPLIST function list+) --> <list>
;;;
($ap 2 "maplist" P.88)
($t (maplist #'append '(1 2 3 4) '(1 2) '(1 2 3)) ((1 2 3 4 1 2 1 2 3) (2 3 4 2 2 3)) equal)
($t (maplist (lambda (x) (cons 'foo x))
 '(a b c d)) ((foo a b c d ) (foo b c d) (foo c d) (foo d))
 equal)
($t (maplist (lambda (x) (if (member (car x) (cdr x)) 0 1)) '(a b a c d b c))
 (0 0 1 0 1 1 1 )
 equal)
;;;
($argc maplist 2 0 1)
($type maplist ($function $generic) :target nil)
($type maplist ($cons $null) (function list) :target)
($type maplist ($cons $null) (function list) (quote (a b)) :target)
;;; 1 引数関数
($t (maplist #'car ()) ())
($t (maplist (lambda (x) x (car 1)) ()) ())
($t (maplist (lambda (x) x) '(1 2 3 4 5)) ((1 2 3 4 5) (2 3 4 5) (3 4 5) (4 5) (5)) equal)
;;; 2 引数関数
($t (maplist #'cons () ()) ())
($t (maplist #'cons () '(a b c)) ())
($t (maplist #'cons '(a b c) ()) ())
($t (maplist (lambda (x y) (list x y)) '(1 2 3) '(4 5 6))
 (((1 2 3) (4 5 6)) ((2 3) (5 6)) ((3) (6)))
 equal)
;;; rest 引数関数
($t (maplist #'list () () () () ()) ())
($t (maplist #'list '(a b c) '(d e f) '(g h i) '(j k l) '(m n o))
 (((a b c) (d e f) (g h i) (j k l) (m n o))
  ((b c) (e f) (h i) (k l) (n o))
  ((c) (f) (i) (l) (o)))
 equal)
($t (maplist #'list '(a b c) '(d e f) '(g) '(j k l) '(m n o))
 (((a b c) (d e f) (g) (j k l) (m n o)))
 equal)
($t (maplist (lambda (&rest x) x) '(1 2 3) '(4 5 6) '(7 8 9))
 (((1 2 3) (4 5 6) (7 8 9))
  ((2 3) (5 6) (8 9))
  ((3) (6) (9)))
 equal)

;;;
;;; 関数 (MAPL function list+) --> <list>
;;;
($ap 2 "mapl" P.88)
($t (let ((k 0))
   (mapl (lambda (x) (setq k (+ k (if (member (car x) (cdr x)) 0 1))))
         '(a b a c d b c))
   k)
 4
 eql)
;;;
($argc mapl 2 0 1)
($type mapl ($function $generic) :target nil)
($type mapl ($cons $null) (function list) :target)
($type mapl ($cons $null) (function list) (quote (a b)) :target)
;;; 1 引数関数
($t (mapl #'car ()) ())
($t (mapl (lambda (x) x (car 1)) ()) ())
($t (let ((ret ()))
   (list (mapl (lambda (x) (setq ret (cons x ret))) '(1 2 3 4 5))
         (nreverse ret)))
 ((1 2 3 4 5) ((1 2 3 4 5) (2 3 4 5) (3 4 5) (4 5) (5))) equal)
;;; 2 引数関数
($t (mapl #'cons () ()) ())
($t (mapl #'cons () '(a b c)) ())
($t (mapl #'cons '(a b c) ()) (a b c) equal)
($t (let ((ret ()))
   (list (mapl (lambda (x y) (setq ret (cons (list x y) ret))) '(1 2 3) '(4 5 6))
         (nreverse ret)))
 ((1 2 3) (((1 2 3) (4 5 6)) ((2 3) (5 6)) ((3) (6))))
 equal)
;;; rest 引数関数
($t (mapl #'list () () () () ()) ())
($t (let ((ret ()))
   (list (mapl (lambda (&rest x) (setq ret (cons x ret))) '(1 2 3) '(4 5 6) '(7 8 9))
         (nreverse ret)))
 ((1 2 3)
  (((1 2 3) (4 5 6) (7 8 9))
   ((2 3) (5 6) (8 9))
   ((3) (6) (9))))
 equal)

;;;
;;; 関数 (MAPCON function list+) --> <list>
;;;
($argc mapcon 2 0 1)
($t (mapcon (lambda (x) (if (member (car x) (cdr x)) (list (car x)))) '(a b a c d b c b c))
 (a b c b c)
 equal)
($t (mapcon #'list '(1 2 3 4)) ((1 2 3 4) (2 3 4) (3 4) (4)) equal)
;;;
($argc mapcon 2 0 1)
($type mapcon ($function $generic) :target nil)
($type mapcon ($cons $null) (function list) :target)
($type mapcon ($cons $null) (function list) (quote (a b)) :target)
;;; 1 引数関数
($t (mapcon #'car ()) ())
($t (mapcon (lambda (x) x (car 1)) ()) ())
($t (mapcon (lambda (x) (list x)) '(1 2 3 4 5)) ((1 2 3 4 5) (2 3 4 5) (3 4 5) (4 5) (5)) equal)
;;; 2 引数関数
($t (mapcon #'cons () ()) ())
($t (mapcon #'cons () '(a b c)) ())
($t (mapcon #'cons '(a b c) ()) ())
($t (mapcon (lambda (x y) (list (list x y))) '(1 2 3) '(4 5 6))
 (((1 2 3) (4 5 6)) ((2 3) (5 6)) ((3) (6)))
 equal)
;;; rest 引数関数
($t (mapcon #'list () () () () ()) ())
($t (mapcon #'list '(a b c) '(d e f) '(g h i) '(j k l) '(m n o))
 ((a b c) (d e f) (g h i) (j k l) (m n o)
  (b c) (e f) (h i) (k l) (n o)
  (c) (f) (i) (l) (o))
 equal)
($t (mapcon #'list '(a b c) '(d e f) '(g) '(j k l) '(m n o))
 ((a b c) (d e f) (g) (j k l) (m n o))
 equal)
($t (mapcon (lambda (&rest x) (list x)) '(1 2 3) '(4 5 6) '(7 8 9))
 (((1 2 3) (4 5 6) (7 8 9))
  ((2 3) (5 6) (8 9))
  ((3) (6) (9)))
 equal)

;;;
;;; 関数 (ASSOC obj association-list) --> <cons>
;;;
($ap 2 "assoc" P.90)
($t (assoc 'a '((a . 1) (b . 2))) (a . 1) equal)
($t (assoc 'a '((a . 1) (a . 2))) (a . 1) equal)
($t (assoc 'c '((a . 1) (b . 2))) nil)
;;;
($argc assoc 2 0 0)
;;;
($t (assoc 'a ()) nil)
($t (assoc 'b '((a) (b) (c))) (b) equal)
($t (assoc 'b '((a 1) (b 2) (c 3))) (b 2) equal)
($t (assoc #\b '((#\a . 1) (#\b . 2) (#\c . 3))) (#\b . 2) equal)
($t (assoc 2 '((1 . 1) (2 . 2) (3 . 3))) (2 . 2) equal)
($t (assoc -2 '((1 . 1) (-2 . 2) (3 . 3))) (-2 . 2) equal)
($t (assoc 2.0 '((1 . 1) (2.0 . 2) (3 . 3))) (2.0 . 2) equal)
($t (assoc -2.0 '((1 . 1) (-2.0 . 2) (3 . 3))) (-2.0 . 2) equal)
($t (assoc 1234567890 '((1 . 1) (1234567890 . 2) (3 . 3))) (1234567890 . 2) equal)
($t (assoc -1234567890 '((1 . 1) (-1234567890 . 2) (3 . 3))) (-1234567890 . 2) equal)
($error (assoc 'a '(a)) <error>)
($error (assoc 'a '(a b c)) <error>)
($t (assoc 'a '((a . b) ())) (a . b) equal)
($t (assoc 'a (create-list 1000 '(a . b))) (a . b) equal)
($t (assoc 'b (create-list 1000 '(a . b))) nil)

