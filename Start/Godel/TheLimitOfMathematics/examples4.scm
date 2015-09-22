(load "Lib/lib.scm")

(newline)
(display "read-bit") (newline)
(try 0 '(read-bit) '(1))
; => (success 1 ())

(newline)
(display "read-exp & read-bits") (newline)
(try 0 '(read-exp) '(0 0 0 0 0 0 0 1 
                     0 0 0 0 1 0 1 0))
; => (success () ())
(try 0 '(read-exp) (append '(0 0 0 0 0 0 0 1 
                             0 0 0 0 1 0 1 0) '(1)))
; => (success () ())
(try 0 '(eval (begin 
                (read-exp) 
                (read-bit))) 
       (append '(0 0 0 0 0 0 0 1 
                 0 0 0 0 1 0 1 0) 
               '(1)))
; => (success 1 ())

(newline)
(display "printC") (newline)
(try 0 '(read-exp) (bits 'a))
; => (success a ())
(try 0 '(eval (read-exp)) (bits '(printC 'a)))
; a
; => (success a (a))

(newline)
(display "pi") (newline)
(define pi
 '(let ((var (bits (read-exp))))
    (try 'no-time-limit '(eval (read-exp)) var)))
; => pi
(try 'no-time-limit '(eval (read-exp))
        (append (bits pi) 
        (append (bits '(printC '(1 X 0))))))
; (1 X 0)
; => (success (success (1 X 0) ((1 X 0))) ())

