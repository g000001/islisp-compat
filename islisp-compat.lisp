;;;; islisp-compat.lisp

(cl:in-package :islisp-compat.internal)
;; (in-readtable :islisp-compat)

(def-suite islisp-compat)

(in-suite islisp-compat)

;;; "islisp-compat" goes here. Hacks and glory await!


(defmacro defsynonym (alias orig &optional doc-string)
  `(progn
     (setf (documentation ',alias 'function)
	   ,doc-string)
     (cl:if (macro-function ',orig)
            (setf (macro-function ',alias) (macro-function ',orig))
            (setf (symbol-function ',alias) (symbol-function ',orig)))))


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
(defsynonym is:function cl:function)
(defsynonym is:lambda cl:lambda)
(defsynonym is:labels cl:labels)
(defsynonym is:flet cl:flet)
(defsynonym is:apply cl:apply)
(defsynonym is:funcall cl:funcall)
(defsynonym is:defconstant cl:defconstant)
;; (defsynonym is:defglobal cl:defglobal)
;; (defsynonym is:defdynamic cl:defdynamic)
(defsynonym is:defun cl:defun)
;; (defsynonym is:t cl:t)
;; (defsynonym is:nil cl:nil)
(defsynonym is:eq cl:eq)
(defsynonym is:eql cl:eql)
(defsynonym is:equal cl:equal)
(defsynonym is:not cl:not)
(defsynonym is:and cl:and)
(defsynonym is:or cl:or)
;; (defsynonym is:constant cl:constant)
(defsynonym is:quote cl:quote)
;; (defsynonym is:var cl:var)
(defsynonym is:setq cl:setq)
(defsynonym is:setf cl:setf)
(defsynonym is:let cl:let)
(defsynonym is:let* cl:let*)
;; (defsynonym is:dynamic cl:dynamic)
;; (defsynonym is:set-dynamic cl:set-dynamic)
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

(defsynonym is:if cl:if)
(defsynonym is:cond cl:cond)
(defsynonym is:case cl:case)
#|(defsynonym is:case-using cl:case-using)|#
(defsynonym is:progn cl:progn)
#|(defsynonym is:while cl:while)|#
(defsynonym is:for cl:do)
(defsynonym is:block cl:block)
(defsynonym is:catch cl:catch)
(defsynonym is:unwind-protect cl:unwind-protect)
(defsynonym is:defclass cl:defclass)
#|(defsynonym is:generic-function-p cl:generic-function-p)|#
#|(defsynonym is:call-next-method cl:call-next-method)|#
#|(defsynonym is:next-method-p cl:next-method-p)|#
#|(defsynonym is:create cl:create)|#
#|(defsynonym is:initialize-object cl:initialize-object)|#
(defsynonym is:class-of cl:class-of)
#|(defsynonym is:instancep cl:instancep)|#
#|(defsynonym is:subclassp cl:subclassp)|#
#|(defsynonym is:class cl:class)|#
(defsynonym is:defmacro cl:defmacro)
(defsynonym is:the cl:the)
#|(defsynonym is:assure cl:assure)|#
#|(defsynonym is:convert cl:convert)|#
(defsynonym is:symbolp cl:symbolp)
#|(defsynonym is:property cl:property)|#
#|(defsynonym is:set-property cl:set-property)|#
#|(defsynonym is:remove-property cl:remove-property)|#
(defsynonym is:gensym cl:gensym)
(defsynonym is:numberp cl:numberp)
#|(defsynonym is:parse-number cl:parse-number)|#
(defsynonym is:= cl:=)
(defsynonym is:/= cl:/=)
(defsynonym is:>= cl:>=)
(defsynonym is:<= cl:<=)
(defsynonym is:> cl:>)
(defsynonym is:< cl:<)
(defsynonym is:+ cl:+)
(defsynonym is:* cl:*)
(defsynonym is:- cl:-)
#|(defsynonym is:quotient cl:quotient)|#
#|(defsynonym is:reciprocal cl:reciprocal)|#
(defsynonym is:max cl:max)
(defsynonym is:min cl:min)
(defsynonym is:abs cl:abs)
(defsynonym is:exp cl:exp)
(defsynonym is:log cl:log)
(defsynonym is:expt cl:expt)
(defsynonym is:sqrt cl:sqrt)
#|(defsynonym is:*pi* cl:pi)|#
(defsynonym is:sin cl:sin)
(defsynonym is:cos cl:cos)
(defsynonym is:tan cl:tan)
(defsynonym is:atan cl:atan)
#|(defsynonym is:atan2 cl:atan2)|#
(defsynonym is:sinh cl:sinh)
(defsynonym is:cosh cl:cosh)
(defsynonym is:tanh cl:tanh)
(defsynonym is:atanh cl:atanh)
#|(defsynonym is:*most-positive-float* cl:*most-positive-float*)|#
#|(defsynonym is:*most-negative-float* cl:*most-negative-float*)|#
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
(defsynonym is:char= cl:char=)
(defsynonym is:char/= cl:char/=)
(defsynonym is:char< cl:char<)
(defsynonym is:char> cl:char>)
(defsynonym is:char<= cl:char<=)
(defsynonym is:char>= cl:char>=)
(defsynonym is:consp cl:consp)
(defsynonym is:cons cl:cons)
(defsynonym is:car cl:car)
(defsynonym is:cdr cl:cdr)
#|(defsynonym is:set-car cl:set-car)|#
#|(defsynonym is:set-cdr cl:set-cdr)|#
(defsynonym is:null cl:null)
(defsynonym is:listp cl:listp)
#|(defsynonym is:create-list cl:create-list)|#
(defsynonym is:list cl:list)
(defsynonym is:reverse cl:reverse)
(defsynonym is:nreverse cl:nreverse)
(defsynonym is:append cl:append)
(defsynonym is:member cl:member)
(defsynonym is:mapcar cl:mapcar)
(defsynonym is:mapc cl:mapc)
(defsynonym is:mapcan cl:mapcan)
(defsynonym is:maplist cl:maplist)
(defsynonym is:mapl cl:mapl)
(defsynonym is:mapcon cl:mapcon)
(defsynonym is:assoc cl:assoc)
#|(defsynonym is:basic-array-p cl:basic-array-p)|#
#|(defsynonym is:basic-array*-p cl:basic-array*-p)|#
#|(defsynonym is:general-array*-p cl:general-array*-p)|#
#|(defsynonym is:create-array cl:create-array)|#
(defsynonym is:aref cl:aref)
#|(defsynonym is:garef cl:garef)|#
#|(defsynonym is:set-aref cl:set-aref)|#
#|(defsynonym is:set-garef cl:set-garef)|#
(defsynonym is:array-dimensions cl:array-dimensions)
#|(defsynonym is:basic-vector-p cl:basic-vector-p)|#
#|(defsynonym is:general-vector-p cl:general-vector-p)|#
#|(defsynonym is:create-vector cl:create-vector)|#
(defsynonym is:vector cl:vector)
(defsynonym is:stringp cl:stringp)
#|(defsynonym is:create-string cl:create-string)|#
#|(defsynonym is:char-index cl:char-index)|#
#|(defsynonym is:string-index cl:string-index)|#
#|(defsynonym is:string-append cl:string-append)|#
(defsynonym is:length cl:length)
(defsynonym is:elt cl:elt)
#|(defsynonym is:set-elt cl:set-elt)|#
(defsynonym is:subseq cl:subseq)
(defsynonym is:map-into cl:map-into)
(defsynonym is:streamp cl:streamp)
(defsynonym is:open-stream-p cl:open-stream-p)
(defsynonym is:input-stream-p cl:input-stream-p)
(defsynonym is:output-stream-p cl:output-stream-p)
#|(defsynonym is:standard-input cl:standard-input)|#
#|(defsynonym is:standard-output cl:standard-output)|#
#|(defsynonym is:error-output cl:error-output)|#
#|(defsynonym is:with-standard-input cl:with-standard-input)|#
#|(defsynonym is:with-standard-output cl:with-standard-output)|#
#|(defsynonym is:with-error-output cl:with-error-output)|#
#|(defsynonym is:open-input-file cl:open-input-file)|#
#|(defsynonym is:open-output-file cl:open-output-file)|#
#|(defsynonym is:open-io-file cl:open-io-file)|#
#|(defsynonym is:with-open-output-file cl:with-open-output-file)|#
#|(defsynonym is:with-open-io-file cl:with-open-io-file)|#
(defsynonym is:close cl:close)
(defsynonym is:finish-output cl:finish-output)
#|(defsynonym is:create-string-input-stream cl:create-string-input-stream)|#
#|(defsynonym is:create-string-output-stream cl:create-string-output-stream)|#
(defsynonym is:get-output-stream-string cl:get-output-stream-string)
(defsynonym is:read cl:read)
(defsynonym is:read-char cl:read-char)
#|(defsynonym is:preview-char cl:preview-char)|#
(defsynonym is:read-line cl:read-line)
#|(defsynonym is:stream-ready-p cl:stream-ready-p)|#
(defsynonym is:format cl:format)
#|(defsynonym is:format-char cl:format-char)|#
#|(defsynonym is:format-float cl:format-float)|#
#|(defsynonym is:format-fresh-line cl:format-fresh-line)|#
#|(defsynonym is:format-integer cl:format-integer)|#
#|(defsynonym is:format-object cl:format-object)|#
#|(defsynonym is:format-tab cl:format-tab)|#
(defsynonym is:read-byte cl:read-byte)
(defsynonym is:write-byte cl:write-byte)
(defsynonym is:probe-file cl:probe-file)
(defsynonym is:file-position cl:file-position)
#|(defsynonym is:set-file-position cl:set-file-position)|#
(defsynonym is:file-length cl:file-length)
(defsynonym is:error cl:error)
(defsynonym is:cerror cl:cerror)
#|(defsynonym is:signal-condition cl:signal-condition)|#
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
#|(defsynonym is:simple-error-format-string cl:simple-error-format-string)|#
#|(defsynonym is:simple-error-format-arguments cl:simple-error-format-arguments)|#
#|(defsynonym is:stream-error-stream cl:stream-error-stream)|#
#|(defsynonym is:undefined-entity-name cl:undefined-entity-name)|#
#|(defsynonym is:undefined-entity-namespace cl:undefined-entity-namespace)|#
(defsynonym is:identity cl:identity)
(defsynonym is:get-universal-time cl:get-universal-time)
(defsynonym is:get-internal-run-time cl:get-internal-run-time)
(defsynonym is:get-internal-real-time cl:get-internal-real-time)
#|(defsynonym is:internal-time-units-per-second cl:internal-time-units-per-second)|#

;;; *EOF*


