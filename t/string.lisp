(cl:in-package :islisp-compat.test)

(in-suite islisp-compat)


;;;
;;;  第１６章  String class
;;;

($ap 1 "String class")

;;;
;;; 関数 (STRINGP obj) --> boolean
;;;
($ap 2 "stringp" P.95)
($t (stringp "abc") t equal)
($t (stringp 'abc) nil)
;;;
($argc stringp 1 0 0)
($predicate stringp $string)

;;;
;;; 関数 (CREATE-STRING i [initial-element]) --> <string>
;;;
($ap 2 "create-string" P.95)
($t (create-string 3 #\a) "aaa" string=)
($t (create-string 0 #\a) "" string=)
;;;
($argc create-string 1 1 0)
($type create-string ($integer) :target)
;;;
($t (create-string 0) "" string=)
#|($t (create-string 1) " " string=) ???|#
($t (length (create-string 1)) 1 =)
($t (length (create-string 1000)) 1000 eql)
($error (create-string -1) <domain-error>)
($error (create-string -1234567890) <domain-error>)
($error (create-string 1234567890) <storage-exhausted>)

;;;
;;; 関数 (STRING= string1 string2) --> quasi-boolean
;;;
($ap 2 "string=" P.96)
($t (if (string= "abcd" "abcd") t nil) t)
($t (if (string= "abcd" "wxyz") t nil) nil)
($t (if (string= "abcd" "abcde") t nil) nil)
($t (if (string= "abcde" "abcd") t nil) nil)
;;;
($argc string= 2 0 0)
($type string= ($string) :target "a")
($type string= ($string) "a" :target)
;;;
($t (string= "" "") t)
($t (string= "" "a") nil)
($t (string= "a" "") nil)
($t (string= "abcd" "Abcd") nil)
($t (string= "abcd" "abcD") nil)
($t (string= (create-string 1000 #\a) (create-string 1000 #\a)) t)
($t (string= (create-string 1000 #\a) (create-string 1000 #\b)) nil)

;;;
;;; 関数 (STRING/= string1 string2) --> quasi-boolean
;;;
($ap 2 "string/=" P.96)
($t (if (string/= "abcd" "wxyz") t nil) t)
;;;
($argc string/= 2 0 0)
($type string/= ($string) :target "a")
($type string/= ($string) "a" :target)
;;;
($t (string/= "" "") nil)
($t (string/= "" "a") t)
($t (string/= "a" "") t)
($t (string/= "abcd" "Abcd") t)
($t (string/= "abcd" "abcD") t)
($t (string/= (create-string 1000 #\a) (create-string 1000 #\a)) nil)
($t (string/= (create-string 1000 #\a) (create-string 1000 #\b)) t)

;;;
;;; 関数 (STRING< string1 string2) --> quasi-boolean
;;;
($ap 2 "string<" P.96)
($t (if (string< "abcd" "abcd") t nil) nil)
($t (if (string< "abcd" "wxyz") t nil) t)
($t (if (string< "abcd" "abcde") t nil) t)
($t (if (string< "abcde" "abcd") t nil) nil)
;;;
($argc string< 2 0 0)
($type string< ($string) :target "a")
($type string< ($string) "a" :target)
;;;
($t (string< "" "") nil)
($t (string< "" "a") t)
($t (string< "a" "") nil)
($t (string< (create-string 1000 #\a) (create-string 1000 #\a)) nil)
($t (string< (create-string 1000 #\a) (create-string 1000 #\b)) t)
($t (string< (create-string 1000 #\b) (create-string 1000 #\a)) nil)

;;;
;;; 関数 (STRING> string1 string2) --> quasi-boolean
;;;
($ap 2 "string>" P.96)
($t (if (string> "abcd" "wxyz") t nil) nil)
;;;
($argc string> 2 0 0)
($type string> ($string) :target "a")
($type string> ($string) "a" :target)
;;;
($t (string> "" "") nil)
($t (string> "" "a") nil)
($t (string> "a" "") t)
($t (string> (create-string 1000 #\a) (create-string 1000 #\a)) nil)
($t (string> (create-string 1000 #\a) (create-string 1000 #\b)) nil)
($t (string> (create-string 1000 #\b) (create-string 1000 #\a)) t)

;;;
;;; 関数 (STRING>= string1 string2) --> quasi-boolean
;;;
($ap 2 "string>=" P.96)
($t (if (string>= "abcd" "abcd") t nil) t)
;;;
($argc string>= 2 0 0)
($type string>= ($string) :target "a")
($type string>= ($string) "a" :target)
;;;
($t (string>= "" "") t)
($t (string>= "" "a") nil)
($t (string>= "a" "") t)
($t (string>= (create-string 1000 #\a) (create-string 1000 #\a)) t)
($t (string>= (create-string 1000 #\a) (create-string 1000 #\b)) nil)
($t (string>= (create-string 1000 #\b) (create-string 1000 #\a)) t)

;;;
;;; 関数 (STRING<= string1 string2) --> quasi-boolean
;;;
($ap 2 "string<=" P.96)
($t (if (string<= "abcd" "abcd") t nil) t)
($t (if (string<= "abcd" "wxyz") t nil) t)
($t (if (string<= "abcd" "abcde") t nil) t)
($t (if (string<= "abcde" "abcd") t nil) nil)
;;;
($argc string<= 2 0 0)
($type string<= ($string) :target "a")
($type string<= ($string) "a" :target)
;;;
($t (string<= "" "") t)
($t (string<= "" "a") t)
($t (string<= "a" "") nil)
($t (string<= (create-string 1000 #\a) (create-string 1000 #\a)) t)
($t (string<= (create-string 1000 #\a) (create-string 1000 #\b)) t)
($t (string<= (create-string 1000 #\b) (create-string 1000 #\a)) nil)

;;;
;;; 関数 (CHAR-INDEX character string [start-position]) --> <object>
;;;
($ap 2 "char-index" P.97)
($t (char-index #\b "abcab") 1 eql)
($t (char-index #\B "abcab") nil)
($t (char-index #\b "abcab" 2) 4 eql)
($t (char-index #\d "abcab") nil)
($t (char-index #\a "abcab" 4) nil)
;;;
($argc char-index 2 1 0)
($type char-index ($character) :target "abc")
($type char-index ($string) #\a :target)
($type char-index ($integer) #\a "abc" :target)
;;;
($t (char-index #\space " ") 0 eql)
($t (char-index #\a "") nil)
($t (char-index #\a (create-string 1000 #\a)) 0 eql)
($t (char-index #\a (create-string 1000 #\b)) nil)
($error (char-index #\a "" 0) <program-error>)
($error (char-index #\a "" -1) <domain-error>)
($error (char-index #\a "" -1234567890) <domain-error>)
($error (char-index #\a "" 1234567890) <program-error>)
($error (char-index #\a "abc" 3) <program-error>)
($error (char-index #\a "abc" -1) <domain-error>)
($error (char-index #\a "abc" -1234567890) <domain-error>)
($error (char-index #\a "abc" 1234567890) <program-error>)

;;;
;;; 関数 (STRING-INDEX substring string [start-position]) --> <object>
;;;
($ap 2 "string-index" P.97)
($t (string-index "foo" "foobar") 0 eql)
($t (string-index "bar" "foobar") 3 eql)
($t (string-index "FOO" "foobar") nil)
($t (string-index "foo" "foobar" 1) nil)
($t (string-index "bar" "foobar" 1) 3 eql)
($t (string-index "foo" "") nil)
($t (string-index "" "foo") 0 eql)
;;;
($argc string-index 2 1 0)
($type string-index ($string) :target "abc")
($type string-index ($string) "abc" :target)
($type string-index ($integer) "abc" "abc" :target)
;;;
($t (string-index " " "a b") 1 eql)
($t (string-index "" "") 0 eql)
($t (string-index "abc" "abc") 0 eql)
($t (string-index "aaa" (create-string 1000 #\a)) 0 eql)
($t (string-index "aaa" (create-string 1000 #\b)) nil)
($error (string-index "abc" "" 0) <program-error>)
($error (string-index "abc" "" -1) <domain-error>)
($error (string-index "abc" "" -1234567890) <domain-error>)
($error (string-index "abc" "" 1234567890) <program-error>)
($error (string-index "a" "abc" 3) <program-error>)
($error (string-index "a" "abc" -1) <domain-error>)
($error (string-index "a" "abc" -1234567890) <domain-error>)
($error (string-index "a" "abc" 1234567890) <program-error>)

;;;
;;; 関数 (STRING-APPEND string*) --> <string>
;;;
($ap 2 "string-append" P.98)
($t (string-append "abc" "def") "abcdef" string=)
($t (string-append "abc" "abc") "abcabc" string=)
($t (string-append "abc" "") "abc" string=)
($t (string-append "" "abc") "abc" string=)
($t (string-append "abc" "" "def") "abcdef" string=)
;;;
($argc string-append 0 0 1)
($type string-append ($string) :target)
($type string-append ($string) :target "abc")
($type string-append ($string) "abc" :target)
($type string-append ($string) :target "abc" "def")
($type string-append ($string) "abc" "def" :target)
;;;
($t (string-append) "" string=)
($t (string-append "" "" "") "" string=)
($t (string-append "abc" "def" "ghi") "abcdefghi" string=)
($t (length (string-append (create-string 1000))) 1000 eql)
($t (length (string-append (create-string 1000) (create-string 1000))) 2000 eql)

