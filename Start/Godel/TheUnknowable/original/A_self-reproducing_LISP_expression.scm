; http://web.archive.org/web/20131029214537/http://cs.umaine.edu/~chaitin/unknowable/fixedpoint.r
; [[[[[
; 
;  A LISP expression that evaluates to itself!
; 
;  Let f(x): x -> (x 'x)
; 
;  Then (f 'f) is a fixed point.
; 
; ]]]]]
(load "Lib/lib.scm")
(load "Lib/lib2.scm")

(newline)
(display "Here is the fixed point done by hand:") (newline)
((lambda (x) `(,x ',x)) '(lambda (x) `(,x ',x)))
; => ((lambda (x) `(,x ',x)) '(lambda (x) `(,x ',x)))

(newline)
(display "Now let's construct the fixed point.") (newline)
(define f (lambda (x) `(,x ',x)))
;(define f (lambda (x) 
;    (let ((y (cons 'quote (cons x nil))))  ; y is 'x
;      (cons (eval y) (cons y nil)))))      ; return (x 'x)

f
; => (\(x) -> `(,x ',x))

(newline)
(display "Here we try f:") (newline)
(f 'x)
; => (x 'x)

(newline)
(display "Here we find the value of the fixed point:") (newline)
(eval (f 'f))
; => (f 'f)

(newline)
(display "Here we check that it's a fixed point:") (newline)
(eq? (f 'f) (eval (f 'f)))
; => #t

(newline)
(display "Just for emphasis:") (newline)
(eq? (f 'f) (eval (eval (eval (eval (eval (eval (f 'f))))))))
; => #t




