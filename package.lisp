;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :islisp-compat
  (:nicknames :is)
  (:use)
  (:import-from :cl :t :nil)
  (:export :&optional :&rest :&key :&allow-other-keys :&aux :&whole :&body
           :&environment)
  (:export
   :functionp :function :lambda :labels :flet :apply :funcall :defconstant
   :defglobal :defdynamic :defun :t :nil :eq :eql :equal :not :and :or 
   :quote :setq :setf :let :let* :dynamic :set-dynamic :dynamic-let :if
   :cond :case :case-using :progn :while :for :block :catch :unwind-protect
   :defclass :generic-function-p :call-next-method :next-method-p :create
   :initialize-object :class-of :instancep :subclassp :class :defmacro :the
   :assure :convert :symbolp :property :set-property :remove-property :gensym
   :numberp :parse-number := :/= :>= :<= :> :< :+ :* :- :div
   :quotient :reciprocal
   :max :min
   :abs :exp :log :expt :sqrt :*pi* :sin :cos :tan :atan :atan2 :sinh :cosh :tanh
   :atanh :*most-positive-float* :*most-negative-float* :floatp :float :floor
   :ceiling :truncate :round :integerp :gcd :lcm :isqrt :characterp :char=
   :char/= :char< :char> :char<= :char>= :consp :cons :car :cdr :set-car :set-cdr
   :null :listp :create-list :list :reverse :nreverse :append :member :mapcar
   :mapc :mapcan :maplist :mapl :mapcon :assoc :basic-array-p :basic-array*-p
   :general-array*-p :create-array :aref :garef :set-aref :set-garef
   :array-dimensions :basic-vector-p :general-vector-p :create-vector :vector
   :stringp :create-string :string= :string/= :string< :string> :string>= 
   :string<=
   :char-index :string-index :string-append :length :elt
   :set-elt :subseq :map-into :streamp :open-stream-p :input-stream-p
   :output-stream-p :standard-input :standard-output :error-output
   :with-standard-input :with-standard-output :with-error-output :open-input-file
   :open-output-file :open-io-file :with-open-input-file
   :with-open-output-file :with-open-io-file
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
   :get-internal-run-time :get-internal-real-time :internal-time-units-per-second)
  (:export 
   :<object> :<basic-array> :<basic-array*> :<general-array*> :<basic-vector>
   :<general-vector> :<string> :<built-in-class> :<character> :<function>
   :<generic-function> :<standard-generic-function> :<list> :<cons> :<null>
   :<symbol> :<number> :<float> :<integer> :<serious-condition> :<error>
   :<arithmetic-error> :<division-by-zero> :<floating-point-overflow>
   :<floating-point-underflow> :<control-error> :<parse-error> :<program-error>
   :<domain-error> :<undefined-entity> :<unbound-variable> :<undefined-function>
   :<simple-error> :<stream-error> :<end-of-stream> :<storage-exhausted>
   :<standard-class> :<standard-object> :<stream>))


(defpackage :islisp-compat.internal
  (:use :cl :named-readtables :fiveam))


(defpackage :islisp-compat.test
  (:use :is :fiveam))

