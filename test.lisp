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


(test arrayp
  (== (mapcar (lambda (x)
                (list (basic-array-p x)
                      (basic-array*-p x)
                      (general-array*-p x)))
              '((a b c)
                "abc"
                #(a b c)
                #1a(a b c)
                #2a((a) (b) (c))))
      '((cl:nil cl:nil cl:nil) 
        (cl:t cl:nil cl:nil) (cl:t cl:nil cl:nil) (cl:t cl:nil cl:nil)
        (cl:t cl:t cl:t))))


(test array1
  (let ((array1 (create-array '(3 3 3) 0)))
    (setf (aref array1 0 1 2) 3.15)
    (== (aref array1 0 1 2) 3.15)
    (set-aref :foo array1 0 2 2)
    (== (aref array1 0 2 2) :foo)))



;;; *EOF*


