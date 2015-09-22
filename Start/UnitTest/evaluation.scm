(unittest "sequencial" (

((let ((a 1))
  (define (f x)
    (define b (+ a x))
    (set! a 5)
    (+ a b))
  (f 10))
; =>
16)
((let ((a 1))
  (define (f x)
    (define b (+ a x))
    (define a 5)
    (+ a b))
  (f 10))
; =>
16)
((let ((a 1))
  (define (f x)
    (define a 5)
    (define b (+ a x))
    (+ a b))
  (f 10))
; =>
20)

((let ()
(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))
(define (factorial n)
  (unless (= n 1)
          (* n (factorial (- n 1)))
          1))
; run forever in applicative-order
; stop in lazy-evaluation
(factorial 5))
; => 
120) 

))
