(cl:in-package :islisp-compat.test)

(def-suite islisp-compat)

(in-suite islisp-compat)

(defmacro == (x y)
  `(is (equal ,x ,y)))


(test case-using
  (== (case-using #'= (+ 1.0 1.0)	
        ((1) 'one)
        (2 'two)
        (t 'more))
      'two)
  (== (case-using #'string= "foo"
        (("fooo" "foo") 'foo)
        (t 'bar))
      'FOO))


(test preview-char 
  (== '(#\f #\f #\o)
      (let ((s (create-string-input-stream "foo")))
        (list (preview-char s) (read-char s) (read-char s)))))


(test parse-number
  (== (parse-number "123.34")
      123.34)
  (== (parse-number"#XFACE")
      64206)
  #|(>_< (parse-number"-37."))|#
  #|(>_< (parse-number"-.5"))|#)


;;; *EOF*


