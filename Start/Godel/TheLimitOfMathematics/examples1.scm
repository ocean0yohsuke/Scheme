(load "Lib/lib.scm")

(newline)
(display "size of S-expression in characters")(newline)
(size 'abc)
; => 3
(size '(a b c))
; => 7

(newline)
(display "number of elements in list")(newline)
(length '(a b c))
; => 3

(newline)
(display "S-expression --> bits")(newline)
(length (print (bits 'a)))
; (0 1 1 0 0 0 0 1 0 0 0 0 1 0 1 0)
; => 16
(length (print (bits 'abc)))
; (0 1 1 0 0 0 0 1 0 1 1 0 0 0 1 0 0 1 1 0 0 0 1 1 0 0 0 0 1 0 1 0)
; => 32
(length (print (bits nil)))
; (0 0 1 0 1 0 0 0 0 0 1 0 1 0 0 1 0 0 0 0 1 0 1 0)
; => 24
(length (print (bits '(a))))
; (0 0 1 0 1 0 0 0 0 1 1 0 0 0 0 1 0 0 1 0 1 0 0 1 0 0 0 0 1 0 1 0)
; => 32

(newline)
(display "leading zeros removed")(newline)
000099
; => 99


