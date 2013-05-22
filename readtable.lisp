;;;; readtable.lisp

(cl:in-package :islisp-compat.internal)
(in-readtable :common-lisp)

#|(defreadtable :islisp-compat  (:merge :standard)
  (:macro-char char fctn opt...)
  (:syntax-from readtable to-char from-char)
  (:case :upcase))|#
