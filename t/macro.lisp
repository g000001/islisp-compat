(cl:in-package :islisp-compat.test)

(in-suite islisp-compat)

;;;
;;; Chapter 8: Macros
;;;

($ap 1 "Macros")

;;;------------------------------------------------------------
($t `(list ,(+ 1 2) 4) (list 3 4) equal)
($t (let ((name 'a)) `(list name ,name ',name)) (list name a 'a) equal)
($t `(a ,(+ 1 2) ,@(create-list 3 'x) b) (a 3 x x x b) equal)
($t `((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))
 ((foo 7) . cons) equal)
($t `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)
 (a `(b ,(+ 1 2) ,(foo 4 d) e) f) equal)
($t (let ((name1 'x)
       (name2 'y))
   `(a `(b ,,name1 ,',name2 d) e))
 (a `(b ,x ,'y d) e) equal)
#|||
;;;------------------------------------------------------------
;;; [defining operator]
;;;
;;;  (defmacro macro-name lambda-list form*) --> <symbol>
;;;------------------------------------------------------------
($t (defmacro my-caar (x) (list 'car (list 'car x))) my-caar)
($t (my-caar '((a b) (c d))) a)
($t (my-caar '(((a b) (c d)) ((e f) (g h)))) (a b) equal)
;;; マクロ定義の中にマクロが含まれる
($eval (defmacro my-first (x) `(car ,x)))
($eval (defmacro my-first2 (x) `(my-first ,x)))
($t (my-first '(1 2)) 1 eql)
($t (my-first2 '(1 2)) 1 eql)
;;; 引数の個数
($error ($eval (defmacro)) <program-error>)
($error ($eval (defmacro foo)) <program-error>)
;;; 引数リストがドットリスト
($error ($eval (defmacro foo 2 . 3)) <error>)
;;; toplevel 定義でない
($error (+ (defmacro foo (x))) <error>)

(cl:defmacro envenv (cl:&environment env)
  (cl:print env))


;;; macro-name がシンボルでない
($error (defmacro #2a((a b) (c d)) ()) <domain-error>)
($error (defmacro #\a ()) <domain-error>)
($error (defmacro 1234()) <domain-error>)
($error (defmacro 123456789 ()) <domain-error>)
($error (defmacro 1.234 ()) <domain-error>)
($error (defmacro "abc" ()) <domain-error>)
($error (defmacro #(a b c) ()) <domain-error>)
($error (defmacro (x y) ()) <domain-error>)
;;; lambda-list に指定された識別子の有効範囲を越えている
($eval (defmacro foo (x) y))
($error (foo 1) <unbound-variable>)

;;;
($eval (defmacro foo (x &rest y) `(list ,x ,@y)))
($t (foo 1) (1) equal)
($t (foo 1 2) (1 2) equal)
($t (foo 1 2 3) (1 2 3) equal)
($argc foo 1 0 1)
($error (foo 1 . 2) <error>)
;;;
($eval (defmacro foo (x y) `(list ,x ,y)))
($t (foo 1 2) (1 2) equal)
($argc foo 2 0 0)
($error ($eval (foo 1 2 . 3)) <error>)

;;; end of file
;|||#
