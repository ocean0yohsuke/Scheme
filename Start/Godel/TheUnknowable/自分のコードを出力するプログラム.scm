; A_self-reproducing_LISP_expression
; http://www.unfindable.net/article/self.html
(load "Lib/lib.scm")

(newline)
(display "quines") (newline)
(define f (lambda (x) `(,x ',x)))
f                          ; => (λ(x) -> `(,x ',x))
(f 'x)                     ; => (x 'x)
(f 'f)                     ; => (f 'f)
(eval (f 'f))              ; => (f 'f)
(eq? (f 'f) (eval (f 'f))) ; => #t

((lambda (x) `(,x ',x)) '(lambda (x) `(,x ',x)))
; => ((lambda (x) `(,x ',x)) '(lambda (x) `(,x ',x)))



