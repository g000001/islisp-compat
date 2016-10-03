;;;; islisp-compat.lisp

(cl:in-package :islisp-compat.internal)
;; (in-readtable :islisp-compat)


;;; "islisp-compat" goes here. Hacks and glory await!

(defmacro check-index (index)
  `(typecase ,index
     ((integer * -1) (error 'is:<domain-error>))))


(defun out-of-memory-p (type size)
  (let ((limit #+sbcl #.(sb-ext:dynamic-space-size)
               #-sbcl 4294967296
               ))
    (<= limit (case type
                ((:list :array) (* 8 size))
                (otherwise size)))))

;;; types

#|(mapc (lambda (x)
        (format t "(deftype is:~A ())~%" (string-downcase x)))
      '(:<object> :<basic-array> :<basic-array*> :<general-array*> :<basic-vector>
  :<general-vector> :<string> :<built-in-class> :<character> :<function>
  :<generic-function> :<standard-generic-function> :<list> :<cons> :<null>
  :<symbol> :<number> :<float> :<integer> :<serious-condition> :<error>
  :<arithmetic-error> :<division-by-zero> :<floating-point-overflow>
  :<floating-point-underflow> :<control-error> :<parse-error> :<program-error>
  :<domain-error> :<undefined-entity> :<unbound-variable> :<undefined-function>
  :<simple-error> :<stream-error> :<end-of-stream> :<storage-exhausted>
  :<standard-class> :<standard-object> :<stream>))|#

(deftype is:<object> () cl:t)
(deftype is:<basic-array> () 'array)
(deftype is:<basic-array*> () '(and array (not (array * (*)))))
(deftype is:<general-array*> () '(and array (not (array * (*)))))
(deftype is:<basic-vector> () 'vector)
(deftype is:<general-vector> () '(and vector (not string)))
(deftype is:<string> () 'string)
(setf (find-class 'is:<string>) (find-class 'string))
(deftype is:<built-in-class> () 'built-in-class)
(deftype is:<character> () 'base-char)
(deftype is:<function> () 'function)
(deftype is:<generic-function> () 'generic-function)
(deftype is:<standard-generic-function> () 'standard-generic-function)
(deftype is:<list> () 'list)
(deftype is:<cons> () 'cons)
(deftype is:<null> () 'null)
(deftype is:<symbol> () 'symbol)
(deftype is:<number> () 'number)
(deftype is:<float> () 'float)
(deftype is:<integer> () 'integer)
(deftype is:<serious-condition> () 'serious-condition)
(define-condition is:<error> (error) ())
(define-condition is:<arithmetic-error> (arithmetic-error) ())
(define-condition is:<division-by-zero> (division-by-zero) ())
(define-condition is:<floating-point-overflow> (floating-point-overflow) ())
(define-condition is:<floating-point-underflow> (floating-point-underflow) ())
(define-condition is:<control-error> (control-error) ())
(define-condition is:<parse-error> (parse-error) ())
(define-condition is:<program-error> (program-error) ())
(define-condition is:<domain-error> (error) ())
(define-condition is:<undefined-entity> (error) ())
(define-condition is:<unbound-variable> (unbound-variable) ())
(define-condition is:<undefined-function> (undefined-function) ())
(define-condition is:<simple-error> (simple-error) ())
(define-condition is:<stream-error> (stream-error) ())
(define-condition is:<end-of-stream> (end-of-file) ())
(define-condition is:<storage-exhausted> (storage-condition) ())
(defclass is:<standard-class> (standard-class) ())
(defclass is:<standard-object> (standard-object) ())
(deftype is:<stream> () 'stream)


(defmacro defsynonym (alias orig &optional doc-string)
  `(progn
     (setf (documentation ',alias 'function)
	   ,doc-string)
     (cl:if (and (symbolp ',orig) (macro-function ',orig))
            (setf (macro-function ',alias) (macro-function ',orig))
            (setf (fdefinition ',alias) (fdefinition ',orig)))))

(defmacro defsynonym-specialform (alias orig &optional doc-string)
  `(progn
     (setf (documentation ',alias 'function)
	   ,doc-string)
     (cl:defmacro ,alias (&rest args) `(,',orig ,@args))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (or (boundp 'isl-lambda-list-keywords)
      (defconstant isl-lambda-list-keywords
        '(is:&optional
          is:&rest
          :rest
          is:&key
          :key
          is:&allow-other-keys
          is:&aux
          :aux
          is:&whole
          is:&body
          is:&environment ))))


(defun normalize-lambda-list-keyword (arg-list)
  (mapcar (lambda (a)
            (case a
              ((is:&optional :optional) 'cl:&optional)
              ((is:&rest :rest) 'cl:&rest)
              ((is:&key :key) 'cl:&key)
              ((is:&allow-other-keys :allow-other-keys) 'cl:&allow-other-keys)
              ((is:&aux :aux) 'cl:&aux)
              ((is:&whole :whole) 'cl:&whole)
              ((is:&body :body) 'cl:&body)
              ((is:&environment :environment) 'cl:&environment)
              (otherwise (if (consp a)
                             (normalize-lambda-list-keyword a)
                             a))))
          arg-list))


#|(mapc (lambda (x)
       (format t "(defsynonym is:~A cl:~:*~A)~%" (string-downcase x)))
     '(:functionp :function :lambda :labels :flet :apply :funcall :defconstant
   :defglobal :defdynamic :defun :t :nil :eq :eql :equal :not :and :or :constant
   :quote :var :setq :setf :let :let* :dynamic :set-dynamic :dynamic-let :if
   :cond :case :case-using :progn :while :for :block :catch :unwind-protect
   :defclass :generic-function-p :call-next-method :next-method-p :create
   :initialize-object :class-of :instancep :subclassp :class :defmacro :the
   :assure :convert :symbolp :property :set-property :remove-property :gensym
   :numberp :parse-number := :/= :>= :<= :> :< :+ :* :- :quotient :reciprocal
   :max :min
   :abs :exp :log :expt :sqrt :*pi* :sin :cos :tan :atan :atan2 :sinh :cosh :tanh
   :atanh :*most-positive-float* :*most-negative-float* :floatp :float :floor
   :ceiling :truncate :round :integerp :gcd :lcm :isqrt :characterp :char=
   :char/= :char< :char> :char<= :char>= :consp :cons :car :cdr :set-car :set-cdr
   :null :listp :create-list :list :reverse :nreverse :append :member :mapcar
   :mapc :mapcan :maplist :mapl :mapcon :assoc :basic-array-p :basic-array*-p
   :general-array*-p :create-array :aref :garef :set-aref :set-garef
   :array-dimensions :basic-vector-p :general-vector-p :create-vector :vector
   :stringp :create-string :char-index :string-index :string-append :length :elt
   :set-elt :subseq :map-into :streamp :open-stream-p :input-stream-p
   :output-stream-p :standard-input :standard-output :error-output
   :with-standard-input :with-standard-output :with-error-output :open-input-file
   :open-output-file :open-io-file :with-open-output-file :with-open-io-file
   :close :finish-output :create-string-input-stream :create-string-output-stream
   :get-output-stream-string :read :read-char :preview-char :read-line
   :stream-ready-p :format :format-char :format-float :format-fresh-line
   :format-integer :format-object :format-tab :read-byte :write-byte :probe-file
   :file-position :set-file-position :file-length :error :cerror
   :signal-condition :ignore-errors :report-condition :condition-continuable
   :with-handler :arithmetic-error-operation :arithmetic-error-operands
   :domain-error-object :domain-error-expected-class :parse-error-string
   :parse-error-expected-class :simple-error-format-string
   :simple-error-format-arguments :stream-error-stream :undefined-entity-name
   :undefined-entity-namespace :identity :get-universal-time
   :get-internal-run-time :get-internal-real-time :internal-time-units-per-second))|#

(defsynonym is:functionp cl:functionp)
(defmacro is:function (name) `(cl:function ,name))
(defmacro is:lambda ((&rest args) &body body)
  `#'(cl:lambda (,@(normalize-lambda-list-keyword args)) ,@body))
(defsynonym-specialform is:labels cl:labels)
(defsynonym-specialform is:flet cl:flet)
(defsynonym is:apply cl:apply)
(defsynonym is:funcall cl:funcall)
(defsynonym is:defconstant cl:defconstant)
(defmacro is:defglobal (var val)
  (let ((backing-var (intern (format nil "-*-global-~A-*-" var))))
    `(progn
       (cl:defconstant ,backing-var
         (if (boundp ',backing-var) (symbol-value ',backing-var) ,val))
       (define-symbol-macro ,var ,backing-var))))
(defmacro is:defglobal (var val)
  (let ((backing-var (intern (format nil "-*-global-~A-*-" var))))
    `(progn
       (cl:defparameter ,backing-var ,val)
       (define-symbol-macro ,var ,backing-var))))
(defsynonym is:defdynamic cl:defvar)
(defmacro is:defun (name (&rest args) &body body)
  `(defun ,name ,(normalize-lambda-list-keyword args) ,@body))
;; (defconstant is:t cl:t)
;; (defconstant is:nil cl:nil)
(defsynonym is:eq cl:eq)
(defsynonym is:eql cl:eql)
(defsynonym is:equal cl:equal)

(defun array-equal (x y)
  (let ((xl (array-total-size x)))
    (and (= xl (array-total-size y))
         (equal (array-dimensions x)
                (array-dimensions y))
         (block nil
           (dotimes (i xl 'T)
             (unless (is:equal (row-major-aref x i)
                               (row-major-aref y i))
               (return nil)))))))

(defun is:equal (x y)
  (block nil
    (unless (eql (class-of x) (class-of y)) (return nil))
    (etypecase x
      (null (null y))
      ((or is:<string>) (cl:equal x y))
      (array (array-equal x y))
      (atom (is:eql x y))
      (cons (and (is:equal (car x) (car y))
                 (is:equal (cdr x) (cdr y)))))))


(defsynonym is:not cl:not)
(defsynonym is:and cl:and)
(defsynonym is:or cl:or)
(defmacro is:quote (thing) `(cl:quote ,thing))
(defmacro is:setq (&rest args) `(cl:setq ,@args))
(defsynonym is:setf cl:setf)
(defmacro is:let ((&rest binds) &body body)
  `(let (,@binds) ,@body))
(defmacro is:let* ((&rest binds) &body body)
  `(let* (,@binds) ,@body))
(defsynonym is:dynamic cl:symbol-value)
(defun (setf is:dynamic) (form var)
  (setf (symbol-value form) var))
(defun is:set-dynamic (form var)
  (setf (symbol-value form) var))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun extract-vars (binds)
    (mapcar (lambda (x)
              (typecase x
                (null nil)
                ((cons symbol null) (car x))
                ((cons symbol (cons * null)) (car x))
                (symbol x)))
            binds)))
(defmacro is:dynamic-let ((&rest binds) &body body)
  (let ((vars (extract-vars binds)))
    `(cl:let (,@binds)
       ,@(and vars `((declare (special ,@vars))))
       ,@body)))
(defmacro is:if (pred then &optional else)
  `(if ,pred ,then ,else))
(defsynonym is:cond cl:cond)
(defsynonym is:case cl:case)
(defmacro is:case-using (predform key &body clauses)
  ;;--- FIXME
  (let ((gkey (gensym "key-"))
        (gpred (gensym "pred-")))
    (labels ((case* (clauses)
               (let ((head (car clauses))
                     (tail (cdr clauses)))
                 (typecase head
                   (null '())
                   ((cons (eql is:t))
                    (cons `(is:t ,@(cdr head))
                          (case* tail)))
                   ((cons cons *)
                    (cons `((member ,gkey ',(car head) :test ,gpred)
                            ,@(cdr head))
                          (case* tail)))
                   ((cons atom *)
                    (cons `((cl:funcall ,gpred ,gkey ,(car head))
                            ,@(cdr head))
                          (case* tail)))))))
      `(let ((,gkey ,key)
             (,gpred ,predform))
         (declare ((or function symbol) ,gpred))
         (cond ,@(case* clauses))))))
(defmacro is:progn (&body body) `(cl:progn ,@body))
(defmacro is:while (pred &body body)
  `(do ()
       ((not ,pred))
     ,@body))
(defsynonym is:for cl:do)
(defmacro is:block (name &body body)
  `(cl:block ,name ,@body))
(defsynonym-specialform is:catch cl:catch)
(defsynonym-specialform is:unwind-protect cl:unwind-protect)
(defsynonym is:defclass cl:defclass)
(defun is:generic-function-p (obj) (typep obj 'cl:generic-function ))
#|(defsynonym is:call-next-method cl:call-next-method)|#
#|(defsynonym is:next-method-p cl:next-method-p)|#
(defsynonym is:create cl:make-instance)
(defsynonym is:initialize-object cl:initialize-instance)
(defsynonym is:class-of cl:class-of)
(defun is:instancep (obj) #+sbcl (sb-kernel:%instancep obj))
(defun is:subclassp (class1 class2)
  (multiple-value-bind (sybtypep validp)
                       (subtypep class1 class2)
    (if validp
        sybtypep
        ;;--- FIXME
        (error "domain-error"))))

(defun make-null-lexenv ()
  #+sbcl (sb-kernel:make-null-lexenv)
  #-sbcl nil)

(defmacro is:class (class-name) `(find-class ',class-name))
(defmacro is:defmacro (name (&rest args) &body body &environment env)
  (if (eq (type-of (make-null-lexenv)) env)
      (is:error "Not a Toplevel Definition:")
      `(defmacro ,name ,(normalize-lambda-list-keyword args) ,@body)))

(defmacro is:the (thing) `(cl:the ,thing))
(defmacro is:assure (class-name form)
  (let ((|obj| (gensym)))
    `(let ((,|obj| ,form))
       (assert (typep ,form ',class-name))
       ,|obj|)))
(defmacro is:convert (obj class-name)
  `(convert-object ,obj ',class-name))

(defgeneric convert-object (obj class))
(defmethod convert-object ((obj t) (class t))
  (coerce obj class))
(defmethod convert-object ((obj t) (class (eql 'is:<string>)))
  (princ-to-string obj))


(defsynonym is:symbolp cl:symbolp)
(defsynonym is:property cl:get)
(defun (setf is:property) (obj symbol property-name)
  (setf (get symbol property-name) obj))
(defun is:set-property (obj symbol property-name)
  (setf (get symbol property-name) obj))
(defun is:remove-property (symbol indicator)
  (second (cl:remprop symbol indicator)))
(defun is:gensym () (cl:gensym))
(defsynonym is:numberp cl:numberp)
(defun is:parse-number (string)
  (let ((num (cl:read-from-string string)))
    ;;--- FIXME
    (assert (numberp num))
    num))
(defsynonym is:= cl:=)
(defsynonym is:/= cl:/=)
(defsynonym is:>= cl:>=)
(defsynonym is:<= cl:<=)
(defsynonym is:> cl:>)
(defsynonym is:< cl:<)
(defsynonym is:+ cl:+)
(defsynonym is:* cl:*)
(defsynonym is:- cl:-)
(defun is:div (x y) (values (floor x y)))
(defun is:quotient (dividend &rest divisors)
  (apply #'/ dividend divisors))
(is:quotient 2 -0.5)
(is:quotient 2 3 4)
(defun is:reciprocal (x) (/ x))
(defsynonym is:max cl:max)
(defsynonym is:min cl:min)
(defsynonym is:abs cl:abs)
(defsynonym is:exp cl:exp)
(defsynonym is:log cl:log)
(defsynonym is:expt cl:expt)
(defsynonym is:sqrt cl:sqrt)
(defconstant is:*pi* cl:pi)
(defsynonym is:sin cl:sin)
(defsynonym is:cos cl:cos)
(defsynonym is:tan cl:tan)
(defun is:atan (x)
  (declare (number x))
  (cl:atan x))
(defun is:atan2 (x y)
  (declare (number x y))
  (cl:atan x y))
(defsynonym is:sinh cl:sinh)
(defsynonym is:cosh cl:cosh)
(defsynonym is:tanh cl:tanh)
(defsynonym is:atanh cl:atanh)
(defconstant is:*most-positive-float* cl:most-positive-single-float)
(defconstant is:*most-negative-float* cl:most-negative-single-float)
(defsynonym is:floatp cl:floatp)
(defsynonym is:float cl:float)
(defsynonym is:floor cl:floor)
(defsynonym is:ceiling cl:ceiling)
(defsynonym is:truncate cl:truncate)
(defsynonym is:round cl:round)
(defsynonym is:integerp cl:integerp)
(defsynonym is:gcd cl:gcd)
(defsynonym is:lcm cl:lcm)
(defsynonym is:isqrt cl:isqrt)
(defsynonym is:characterp cl:characterp)
(defun is:char= (x y) (cl:char= x y))
(defun is:char/= (x y) (cl:char/= x y))
(defun is:char< (x y) (cl:char< x y))
(defun is:char> (x y) (cl:char> x y))
(defun is:char<= (x y) (cl:char<= x y))
(defun is:char>= (x y) (cl:char>= x y))
(defsynonym is:consp cl:consp)
(defsynonym is:cons cl:cons)
(defun is:car (cons)
  (when (null cons) (error 'is:<domain-error>))
  (cl:car cons))
(defun is:cdr (cons)
  (when (null cons) (error 'is:<domain-error>))
  (cl:cdr cons))
(defun (setf is:car) (val var) (funcall #'(setf car) val var))
(defun (setf is:cdr) (val var) (funcall #'(setf cdr) val var))
(defun is:set-car (val var) (funcall #'(setf car) val var))
(defun is:set-cdr (val var) (funcall #'(setf cdr) val var))
(defsynonym is:null cl:null)
(defsynonym is:listp cl:listp)
(defun is:create-list (i &optional initial-element)
  (check-index i)
  (when (out-of-memory-p :list i) (error 'is:<storage-exhausted>))
  (cl:make-list i :initial-element initial-element))
(defsynonym is:list cl:list)
(defsynonym is:reverse cl:reverse)
(defsynonym is:nreverse cl:nreverse)
(defsynonym is:append cl:append)
(defun is:member (key list) (cl:member key list))
(defsynonym is:mapcar cl:mapcar)
(defsynonym is:mapc cl:mapc)
(defsynonym is:mapcan cl:mapcan)
(defsynonym is:maplist cl:maplist)
(defsynonym is:mapl cl:mapl)
(defsynonym is:mapcon cl:mapcon)
(defun is:assoc (key alist)
  (handler-case (cl:assoc key alist)
    (type-error () (error 'is:<error>))))
(defsynonym is:basic-array-p cl:arrayp)
(defun is:basic-array*-p (obj) (typep obj 'is:<basic-array*>))
(defun is:general-array*-p (obj) (typep obj 'is:<general-array*>))
(defun is:create-array (dimensions &optional (initial-element 0))
  (unless (every (lambda (x)
                   (typep x '(integer 0 *)))
                 dimensions)
    (error 'is:<domain-error>))
  (when (out-of-memory-p :array (apply #'* (remove 0 dimensions)))
    (error 'is:<storage-exhausted>))
  (make-array dimensions :initial-element initial-element))


(defun is:aref (array &rest subscripts)
  (unless (every (lambda (x)
                   (typep x '(integer 0 *)))
                 subscripts)
    (error 'is:<domain-error>))
  (handler-case (apply #'aref array subscripts)
    ((or simple-error #+sbcl sb-int:invalid-array-index-error) ()
      (error 'is:<program-error>))))

(defsynonym is:garef is:aref)
(defun is:set-aref (obj basic-array &rest z)
  (unless (every (lambda (x) (typep x '(integer 0 *))) z)
    (error 'is:<domain-error>))
  (handler-case (setf (apply #'aref basic-array z) obj)
    ((or simple-error #+sbcl sb-int:invalid-array-index-error) ()
      (error 'is:<program-error>))))
(defun (setf is:aref) (obj basic-array &rest z)
  (apply #'is:set-aref obj basic-array z))
(defun is:set-garef (obj array &rest z)
  (apply #'is:set-aref obj array z))
(defun (setf is:garef) (obj array &rest z)
  (apply #'is:set-aref obj array z))
(defsynonym is:array-dimensions cl:array-dimensions)
(defun is:basic-vector-p (obj)
  (typep obj 'is:<basic-vector>))
(defun is:general-vector-p (obj)
  (typep obj 'is:<general-vector>))
(defun is:create-vector (i &optional (initial-element 0))
  (unless (and (integerp i) (typep i '(integer 0 *)))
    (error 'is:<domain-error>))
  (handler-case (make-sequence 'vector i :initial-element initial-element)
    #+sbcl (sb-kernel::heap-exhausted-error () (error 'is:<storage-exhausted>))
    (error (c) (error c))))
(defsynonym is:vector cl:vector)
(defsynonym is:stringp cl:stringp)
(defun is:create-string (i &optional (initial-character #\Nul))
  (unless (and (integerp i) (typep i '(integer 0 *)))
    (error 'is:<domain-error>))
  (handler-case (make-string i :initial-element initial-character)
    #+sbcl (sb-kernel::heap-exhausted-error () (error 'is:<storage-exhausted>))
    (error (c) (error c))))
(defun is:string= (x y) (cl:string= x y))
(defun is:string/= (x y) (and (cl:string/= x y) t))
(defun is:string< (x y) (and (cl:string< x y) t))
(defun is:string> (x y) (and (cl:string> x y) t))
(defun is:string>= (x y) (and (cl:string>= x y) t))
(defun is:string<= (x y) (and (cl:string<= x y) t))
(defun is:char-index (char string &optional (start-position 0 sp?))
  (declare (character char)
           (string string))
  (when (typep start-position '(not (integer 0 *)))
    (error 'is:<domain-error>))
  (when (and sp? (or (typep string '(string 0))
                     (>= start-position (length string))))
    (error 'is:<program-error>))
  ;;
  (position char string :start start-position))
(defun is:string-index (substring string &optional (start-position 0 sp?))
  (declare (string substring string))
  (when (typep start-position '(not (integer 0 *)))
    (error 'is:<domain-error>))
  (when (and sp? (or (typep string '(string 0))
                     (>= start-position (length string))))
    (error 'is:<program-error>))
  (search substring string :start2 start-position))
(defun is:string-append (&rest strings)
  (let* ((anslen (reduce (lambda (res x) (+ (length x) res)) strings
                         :initial-value 0))
         (ans (make-string anslen))
         (ansidx -1))
    (dolist (s strings ans)
      (loop :for c :across s
            :do (setf (char ans (incf ansidx)) c)))))
(defsynonym is:length cl:length)
(defsynonym is:elt cl:elt)
(defun is:elt (sequence index)
  (when (>= index (length sequence))
    (error 'is:<program-error>))
  (unless (typep index '(integer 0 *))
    (error 'is:<domain-error>))
  (elt sequence index))
(defun (setf is:elt) (obj sequence index)
  (is:set-elt obj sequence index))
(defun is:set-elt (obj sequence z)
  (when (>= z (length sequence))
    (error 'is:<program-error>))
  (unless (typep z '(integer 0 *))
    (error 'is:<domain-error>))
  (setf (elt sequence z) obj))
(defun is:subseq (sequence start end)
  (cl:subseq sequence start end))
(defsynonym is:map-into cl:map-into)
(defsynonym is:streamp cl:streamp)
(defsynonym is:open-stream-p cl:open-stream-p)
(defsynonym is:input-stream-p cl:input-stream-p)
(defsynonym is:output-stream-p cl:output-stream-p)
(defun is:standard-input () cl:*standard-input*)
(defun is:standard-output () cl:*standard-output*)
(defun is:error-output () cl:*error-output*)
(defmacro is:with-standard-input (stream-form &body forms)
  `(with-open-stream (cl:*standard-input* ,stream-form)
     ,@forms))
(defmacro is:with-standard-output (stream-form &body forms)
  `(with-open-stream (cl:*standard-output* ,stream-form)
     ,@forms))
(defmacro is:with-error-output (stream-form &body forms)
  `(with-open-stream (cl:*error-output* ,stream-form)
     ,@forms))
(defun is:open-input-file (filename &optional (element-class 'base-char))
  (open filename :direction :input :element-type element-class))
(defun is:open-output-file (filename &optional (element-class 'base-char))
  (open filename :direction :output :element-type element-class))
(defun is:open-io-file (filename &optional (element-class 'base-char))
  (open filename :direction :io :element-type element-class))
(defmacro is:with-open-input-file
          ((name filename &optional (element-class 'is:<character>))
           &body body)
  `(with-open-file (,name ,filename
                          :direction :input
                          :element-type ',element-class)
     ,@body))
(defmacro is:with-open-output-file
          ((name filename &optional (element-class 'is:<character>))
           &body body)
  `(with-open-file (,name ,filename
                          :direction :output
                          :element-type ',element-class)
     ,@body))
(defmacro is:with-open-io-file ((name filename
                                      &optional (element-class 'is:<character>))
                                &body body)
  `(with-open-file (,name ,filename
                          :direction :io
                          :element-type ',element-class)
     ,@body))
(defsynonym is:close cl:close)
(defsynonym is:finish-output cl:finish-output)
(defsynonym is:create-string-input-stream cl:make-string-input-stream)
(defsynonym is:create-string-output-stream cl:make-string-output-stream)
(defsynonym is:get-output-stream-string cl:get-output-stream-string)
(defsynonym is:read cl:read)
(defsynonym is:read-char cl:read-char)
(defun is:preview-char
       (&optional (input-stream *standard-input*) (eos-error-p t) eos-value)
  (peek-char nil input-stream eos-error-p eos-value))
(defsynonym is:read-line cl:read-line)
(defun is:stream-ready-p (stream)
  #+swank
  (swank-backend::input-ready-p stream))
(defsynonym is:format cl:format)
(defun is:format-char (output-stream char)
  (format output-stream "~C" char))
(defun is:format-float (output-stream float)
  (format output-stream "~F" float))
(defun is:format-fresh-line (output-stream)
  (cl:fresh-line output-stream))
(defun is:format-integer (output-stream integer)
  (format output-stream "~D" integer))
(defun is:format-object (output-stream obj escape-p)
  (write obj :stream output-stream :escape escape-p))
(defun is:format-tab (output-stream column)
  (format output-stream "~VT" column))
(defsynonym is:read-byte cl:read-byte)
(defsynonym is:write-byte cl:write-byte)
(defsynonym is:probe-file cl:probe-file)
(defun is:file-position (stream) (cl:file-position stream))
(defun is:set-file-position (stream z) (cl:file-position stream z))
(defsynonym is:file-length cl:file-length)
(defsynonym is:error cl:error)
(defsynonym is:cerror cl:cerror)
(defun is:signal-condition (condition continuable)
  (if continuable
      (error condition)
      (cerror (or continuable "") condition)))
(defsynonym is:ignore-errors cl:ignore-errors)
#|(defsynonym is:report-condition cl:report-condition)|#
#|(defsynonym is:condition-continuable cl:condition-continuable)|#
#|(defsynonym is:with-handler cl:with-handler)|#
#|(defsynonym is:arithmetic-error-operation cl:arithmetic-error-operation)|#
#|(defsynonym is:arithmetic-error-operands cl:arithmetic-error-operands)|#
#|(defsynonym is:domain-error-object cl:domain-error-object)|#
#|(defsynonym is:domain-error-expected-class cl:domain-error-expected-class)|#
#|(defsynonym is:parse-error-string cl:parse-error-string)|#
#|(defsynonym is:parse-error-expected-class cl:parse-error-expected-class)|#
(defsynonym is:simple-error-format-string cl:simple-condition-format-control)
(defsynonym is:simple-error-format-arguments cl:simple-condition-format-arguments)
#|(defsynonym is:stream-error-stream cl:stream-error-stream)|#
#|(defsynonym is:undefined-entity-name cl:undefined-entity-name)|#
#|(defsynonym is:undefined-entity-namespace cl:undefined-entity-namespace)|#
(defsynonym is:identity cl:identity)
(defsynonym is:get-universal-time cl:get-universal-time)
(defsynonym is:get-internal-run-time cl:get-internal-run-time)
(defsynonym is:get-internal-real-time cl:get-internal-real-time)
(defun is:internal-time-units-per-second () cl:internal-time-units-per-second)

;;; *EOF*
