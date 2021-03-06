(load "Lib/lib.scm")

(stacktracelength 0)

(newline)
(display "illustrate read-bit & read-exp") (newline)
(read-bit)
; => *** out-of-data
(read-exp)
; => *** out-of-data
(try 0 '(read-bit) nil)
; => (failure out-of-data ())
(try 0 '(read-exp) (bits 'abc))
; => (success abc ())
(try 0 '(read-exp) (bits '(abc def)))
; => (success (abc def) ())
(try 0 '(read-exp) (bits '(abc (def ghi) jkl)))
; => (success (abc (def ghi) jkl) ())
(try 0 '(cons (read-exp) (cons (read-bit) nil)) 
       (bits 'abc))
; => (failure out-of-data ())
(try 0 '(cons (read-exp) (cons (read-bit) nil))
       (append (bits 'abc) '(0)))
; => (success (abc 0) ())
(try 0 '(cons (read-exp) (cons (read-bit) nil))
       (append (bits 'abc) '(1)))
; => (success (abc 1) ())
(try 0 '(read-exp) (bits '(a b c)))
; => (success (a b c) ())
(try 0 '(cons (read-exp) (cons (read-exp) nil)) 
       (bits '(a b c)))
; => (failure out-of-data ())
(try 0 '(cons (read-exp) (cons (read-exp) nil))
       (append (bits '(a b c)) (bits '(d e f))))
; => (success ((a b c) (d e f)) ())

(newline)

(display "to get characters codes") (newline)
(bits 'a)
; => (0 1 1 0 0 0 0 1 0 0 0 0 1 0 1 0)
(display "'a' but no \n character") (newline)
(try 0 '(read-exp) '(0 1 1 0 0 0 0 1))
; => (failure out-of-data ())
(try 0 '(read-exp) '(0 1 1 0 0 0 0 1 0 0 0 0 1 0 1))
; => (failure out-of-data ())
(try 0 '(read-exp) '(0 1 1 0 0 0 0 1 0 0 0 0 1 0 1 0))
; => (success a ())

(newline)
(display "if we get to \n reading 8 bits at a time, we will always interpret as a valid S-expression") (newline)
(try 0 '(read-exp) '(0 0 0 0 1 0 1 0))
; => (success () ())
(display "unprintable character is deleted") (newline)
(try 0 '(read-exp) '(1 1 1 1 1 1 1 1 0 0 0 0 1 0 1 0))
; => (success () ())


(newline)

(display "to get characters codes") (newline)
(bits ())
; => (0 0 1 0 1 0 0 0 0 0 1 0 1 0 0 1 0 0 0 0 1 0 1 0)
(display "((( ==> ((()))") (newline)
(try 0 '(read-exp) '(0 0 1 0 1 0 0 0 0 0 1 0 1 0 0 0 0 0 1 0 1 0 0 0 0 0 0 0 1 0 1 0))
; => (success parse-error ())
(display ")a ==> ()") (newline)
(try 0 '(read-exp) '(0 0 1 0 1 0 0 1 0 1 1 0 0 0 0 1 0 0 0 0 1 0 1 0))
; => (success parse-error ())
(display "a) ==> a") (newline)
(try 0 '(read-exp) '(0 1 1 0 0 0 0 1 0 0 1 0 1 0 0 1 0 0 0 0 1 0 1 0))
; => (success parse-error ())


