(cl:in-package :islisp-compat.test)

(in-suite islisp-compat)

;;;
;;;  第１２章  Character class
;;;

($ap 1 "Character class")

;;;
;;; 関数 (CHARACTERP obj) --> boolean
;;;
($ap 2 "characterp" P.82)
($t (characterp #\a) t)
($t (characterp "a") nil)
($t (characterp 'a) nil)
;;;
($argc characterp 1 0 0)
($predicate characterp $character)
;;;
($t (characterp #\newline) t)
($t (characterp #\space) t)

;;;
;;; 関数 (CHAR= char1 char2) --> boolean
;;;
($ap 2 "char=" P.82)
($t (char= #\a #\a) t)
($t (char= #\a #\b) nil)
($t (char= #\a #\A) nil)
;;;
($argc char= 2 0 0)
($type char= ($character) :target #\a)
($type char= ($character) #\a :target)
;;;
($t (char= #\newline #\newline) t)
($t (char= #\space #\space) t)

;;;
;;; 関数 (CHAR/= char1 char2) --> boolean
;;;
($ap 2 "char/=" P.82)
($t (char/= #\a #\a) nil)
;;;
($argc char/= 2 0 0)
($type char/= ($character) :target #\a)
($type char/= ($character) #\a :target)
;;;
($t (char/= #\newline #\newline) nil)
($t (char/= #\newline #\space) t)
($t (char/= #\a #\A) t)

;;;
;;; 関数 (CHAR< char1 char2) --> boolean
;;;
($ap 2 "char<" P.82)
($t (char< #\a #\a) nil)
($t (char< #\a #\b) t)
($t (char< #\b #\a) nil)
;;((char< #\a #\A) nil)					; nil or t (IDEF)
;;((char< #\* #\a) t)					; nil or t (IDEF)
;;;
($argc char< 2 0 0)
($type char< ($character) :target #\a)
($type char< ($character) #\a :target)
;;;
($t (char< #\a #\z) t)
($t (char< #\A #\A) nil)
($t (char< #\A #\Z) t)
($t (char< #\0 #\0) nil)
($t (char< #\0 #\9) t)

;;;
;;; 関数 (CHAR> char1 char2) --> boolean
;;;
($ap 2 "char>" P.82)
($t (char> #\b #\a) t)
;;;
($argc char> 2 0 0)
($type char> ($character) :target #\a)
($type char> ($character) #\a :target)
;;;
($t (char> #\a #\a) nil)
($t (char> #\z #\a) t)
($t (char> #\A #\A) nil)
($t (char> #\Z #\A) t)
($t (char> #\0 #\0) nil)
($t (char> #\9 #\0) t)

;;;
;;; 関数 (CHAR<= char1 char2) --> boolean
;;;
($ap 2 "char<=" P.82)
($t (char<= #\a #\a) t)
;;((char<= #\a #\A) nil)				; nil or t (IDEF)
;;;
($argc char<= 2 0 0)
($type char<= ($character) :target #\a)
($type char<= ($character) #\a :target)
;;;
($t (char<= #\a #\z) t)
($t (char<= #\A #\A) t)
($t (char<= #\A #\Z) t)
($t (char<= #\0 #\0) t)
($t (char<= #\0 #\9) t)

;;;
;;; 関数 (CHAR>= char1 char2) --> boolean
;;;
($ap 2 "char>=" P.82)
($t (char>= #\b #\a) t)
($t (char>= #\a #\a) t)
;;;
($argc char>= 2 0 0)
($type char>= ($character) :target #\a)
($type char>= ($character) #\a :target)
;;;
($t (char>= #\z #\a) t)
($t (char>= #\A #\A) t)
($t (char>= #\Z #\A) t)
($t (char>= #\0 #\0) t)
($t (char>= #\9 #\0) t)
