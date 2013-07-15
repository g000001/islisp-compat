(cl:in-package :islisp-compat.test)

(in-suite islisp-compat)


;;;
;;;  第２２章  Miscellaneous
;;;

($ap 1 "Miscellaneous")

#+ansi-cl
($eval (defun tak (x y z)
         (cl:sleep 1.1)))

#-ansi-cl
($eval (defun tak (x y z)
         (if (not (< y x))
             z
             (tak (tak (- x 1) y z)
                  (tak (- y 1) z x)
                  (tak (- z 1) x y)))))


;;;
;;; 関数 (IDENTITY obj) --> <object>
;;;
($ap 2 "identity" P.120)
($t (let ((x '(a b c))) (eql (identity x) x)) t)
;;;
($argc identity 1 0 0)
;;;
($t (let ((x "abc")) (eql (identity x) x)) t)
($t (let ((x #(a b c))) (eql (identity x) x)) t)

;;;
;;; 関数 (GET-UNIVERSAL-TIME) --> <integer>
;;;
($ap 2 "get-universal-time" P.120)
;;;
($argc get-universal-time 0 0 0)
;;;
($t (let ((time (get-universal-time))) (and (integerp time) (< 0 time))) t)
($t (let ((t1 (get-universal-time))
          (dummy1 (tak 21 14 7))
          (dummy2 (tak 21 14 7))
          (dummy3 (tak 21 14 7))
          (t2 (get-universal-time)))
      dummy1 dummy2 dummy3
      (< t1 t2))
    t)

;;;
;;; 関数 (GET-INTERNAL-RUN-TIME) --> <integer>
;;;
($ap 2 "get-internal-run-time" P.121)
;;;
($argc get-internal-run-time 0 0 0)
;;;
($t (let ((time (get-internal-run-time))) (and (integerp time) (< 0 time))) t)
($t (let ((t1 (get-internal-run-time))
          (dummy (tak 18 12 6))
          (t2 (get-internal-run-time)))
      dummy
   (< t1 t2))
 t)


;;;
;;; 関数 (GET-INTERNAL-REAL-TIME) --> <integer>
;;;
($ap 2 "get-internal-real-time" P.121)
;;;
($argc get-internal-real-time 0 0 0)
;;;
($t (let ((time (get-internal-real-time))) (and (integerp time) (< 0 time))) t)
($t (let ((t1 (get-internal-real-time))
       (dummy (tak 18 12 6))
       (t2 (get-internal-real-time)))
      dummy
   (< t1 t2))
 t)

;;;
;;; 関数 (internal-time-units-per-second) --> <integer>
;;;
($ap 2 "internal-time-units-per-second" P.121)
;;;
($argc internal-time-units-per-second 0 0 0)
;;;
($t (< 0 (internal-time-units-per-second)) t)


;;; *EOF*
