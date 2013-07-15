(cl:in-package :islisp-compat.test)

(in-suite islisp-compat)

;;;
;;;  第１０章  Symbol class
;;;

($ap 1 "Symbol class")

;;;
;;; 関数 (SYMBOLP obj) --> boolean
;;;
($ap 2 "symbolp" P.63)
($t (symbolp 'a) t)
($t (symbolp "a") nil)
($t (symbolp #\a) nil)
($t (symbolp 't) t)
($t (symbolp t) t)
($t (symbolp 'nil) t)
($t (symbolp nil) t)
($t (symbolp '()) t)
($t (symbolp '*pi*) t)
($t (symbolp *pi*) nil)
;;;
($argc symbolp 1 0 0)
($predicate symbolp $symbol $t $null)

;;;
;;; 関数 (PROPERTY symbol property-name [obj]) --> <object>
;;;
($ap 2 "property" P.65)
;;;
($argc property 2 1 0)
($type property ($symbol $null) :target (quote abc))
($type property ($symbol $null) (quote abc) :target)
;;;
($t (property 'aaa 'bbb) nil)
($t (property 'aaa 'bbb 'zzz) zzz)

;;;
;;; 関数 (SET-PROPERTY obj symbol property-name) --> <object>
;;;
($ap 2 "set-property" P.66)
($t (setf (property 'zeus 'daughter) 'athena) athena)
($t (set-property 'athena 'zeus 'daughter) athena)
($t (property 'zeus 'daughter) athena)
;;;
($argc set-property 3 0 0)
($type set-property ($symbol $null) (quote a) :target (quote abc))
($type set-property ($symbol $null) (quote a) (quote abc) :target)
;;;
($t (list (setf (property 'aaa 'bbb) 'ccc) (property 'aaa 'bbb 'zzz)) 
 (ccc ccc) equal)
($t (list (set-property 'ccc 'aaa 'bbb) (property 'aaa 'bbb 'zzz)) 
 (ccc ccc) equal)

;;;
;;; 関数 (REMOVE-PROPERTY symbol property-name) --> <object>
;;;
($ap 2 "remove-property" P.66)
($t (remove-property 'zeus 'daughter) athena)
;;;
($argc remove-property 2 0 0)
($type remove-property ($symbol $null) :target (quote abc))
($type remove-property ($symbol $null) (quote abc) :target)
;;;
($t (list (remove-property 'aaa 'bbb) (property 'aaa 'bbb 'zzz)) 
 (ccc zzz) equal)

;;;
;;; 関数 (GENSYM) --> <symbol>
;;;
($ap 2 "gensym" P.66)
($eval (defmacro twice (x)
         (let ((v (gensym)))
           `(let ((,v ,x)) (+ ,v ,v)))))
($t (twice 5) 10 eql)
;;;
($argc gensym 0 0 0)
;;;
($t (symbolp (gensym)) t)
