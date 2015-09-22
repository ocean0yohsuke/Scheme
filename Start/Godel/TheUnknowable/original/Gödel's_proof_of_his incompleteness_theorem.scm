; http://web.archive.org/web/20131029220009/http://cs.umaine.edu/~chaitin/unknowable/godel.r
; [[[[[
; 
;  A LISP expression that asserts that it itself is unprovable!
; 
;  Let g(x): x -> (is-unprovable (value-of (x 'x)))
; 
;  Then (is-unprovable (value-of (g 'g)))
;  asserts that it itself is not a theorem!
; 
; ]]]]]
(load "Lib/lib.scm")
(load "Lib/lib2.scm")

;(define g (lambda (x) `(is-unprovable (value-of (,x ',x)))))
(define (g x) 
  (let* ((L (lambda (x y) (cons x (cons y nil))))      ; Makes x and y into list.
         (y (L 'quote x)))
    (L 'is-unprovable (L 'value-of (L (eval y) y)))))

(newline)
(display "Here we try g:") (newline)
(g 'x)
; => (is-unprovable (value-of (x 'x)))

(newline)
(display 
"Here we calculate the LISP expression 
that asserts its own unprovability:") (newline)
(g 'g)
; => (is-unprovable (value-of (g 'g)))

(newline)
(display "Here we extract the part that it uses to name itself:") (newline)
(cadr (cadr (g 'g))) 
; => (g 'g)

(newline)
(display "Here we evaluate the name to get back the entire expression:") (newline)
(eval (cadr (cadr (g 'g)))) 
; => (is-unprovable (value-of (g 'g)))

(newline)
(display "Here we check that it worked:") (newline)
(eq? (g 'g) (eval (cadr (cadr (g 'g)))))
; => #t





