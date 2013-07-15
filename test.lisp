(cl:in-package :islisp-compat.test)

(def-suite islisp-compat)

(in-suite islisp-compat)

(defglobal *tp-all-types* 
  (list
   (cons '$array		(create-array '(1 2 3) nil))
   (cons '$vector		(vector 1 2 3))
   (cons '$string		(string-append (cl:string (gensym)) "islsp0"))
   #|(cons '$class		(class <object>))|#
   (cons '$character		#\x)
   (cons '$function		'(function +))
   (cons '$generic		'(function create))
   (cons '$symbol		''aBcD)
   (cons '$cons			(list 'list 1 2))
   (cons '$null			''nil)
   (cons '$float		1.234)
   (cons '$integer		0)
   #|(cons '$arithmetic-error	*tp-arithmetic-error*)|#
   #|(cons '$domain-error		*tp-domain-error*)|#
   #|(cons '$parse-error		*tp-parse-error*)|#
   #|(cons '$simple-error		*tp-simple-error*)|#
   #|(cons '$stream-error		*tp-stream-error*)|#
   #|(cons '$undefined-entity	*tp-undefined-entity*)|#
   #|(cons '$instance		*tp-instance*)|#
   #|(cons '$file-input-stream  
	 (with-open-input-file 
	  (istr *tp-tmp-input-file*) istr))|#
   #|(cons '$file-output-stream 
	 (with-open-output-file 
	  (ostr *tp-tmp-output-file*) ostr))|#
   #|(cons '$string-input-stream	(create-string-input-stream "hello"))|#
   #|(cons '$string-output-stream	(create-string-output-stream))|#
   ))


(defmacro == (x y)
  `(is (equal ,x ,y)))


(defmacro $ap (:rest args)
  (cl:declare (cl:ignore args))
  nil)

(defmacro $type (func-name (:rest ok-types) :rest args)
  `(test ,(cl:intern (format nil "$type-~A" func-name))
     ,@(mapcar (lambda (x) 
                 (let* ((target (cdr (assoc x *tp-all-types*)))
                        (args (cl:substitute target :target args)))
                   `(is-true (progn (,func-name ,@args) t))))
               ok-types)))

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (defun arglist-reqs-opt-rest (arglist)
    (let ((reqs (or (cl:position-if (lambda (x)
                                      (member x '(cl:&optional cl:&rest)))
                                    arglist)
                    (length arglist)))
          (opt (max (+ -1 (length (member 'cl:&optional arglist)))
                    0))
          (rest (max (+ -1 (length (member 'cl:&rest arglist)))
                     0)))
      (list reqs opt rest)))


  (defun chk-arglist (func-name ror-list)
    (let ((arglist #+sbcl (sb-introspect:function-lambda-list func-name)))
      (equal (arglist-reqs-opt-rest arglist) ror-list))))


(defmacro $argc (func-name reqs opt rest)
  `(test ,(cl:intern (format nil "$ARGC-~A" func-name))
     (is-true (chk-arglist ',func-name (list ,reqs ,opt ,rest)))))

(defmacro $eval (form)
  `(cl:eval-when (:compile-toplevel :load-toplevel :execute)
     (cl:eval ',form)))

(defmacro $error (form condition-class-name)
  `(test ,(cl:intern (format nil "$error-~A" form))
     (signals (,condition-class-name)
       ,form)))


(defmacro $$error (:rest args)
  (cl:declare (cl:ignore args))
  nil)

(defmacro $$type (:rest args)
  (cl:declare (cl:ignore args))
  nil)

(defmacro $predicate (:rest args)
  (cl:declare (cl:ignore args))
  nil)


(defmacro $t (form correct :optional (pred-func 'eq))
  `(test ,(cl:intern (cl:write-to-string form :pretty nil))
     (is-true (funcall ',pred-func ,form ',correct))))


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


(test with-standard-input
  (== (with-standard-input (create-string-input-stream "this is a string")
        (list (read) (read)))
      '(THIS IS)))

#|(with-standard-output (create-string-output-stream)
  (cl:print "foo")
  (cl:print "bar")
  (cl:get-output-stream-string (standard-output)))|#


#|(signal-condition
 (create (class <simple-error>)
         'format-string error-string
         'format-arguments (list obj ))
 nil)|#

#|(signal-condition
 (cl:make-condition 'cl:simple-error
                    :format-control "foo"
                    :format-arguments '(8))
  nil)|#

;; (cl:make-instance 'cl:simple-error)


;;; *EOF*


