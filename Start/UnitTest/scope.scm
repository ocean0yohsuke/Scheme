
(unittest "let" (

(
(let ((a 1))
  (let ((b 1))
    (set! a 3)
  )
a)
; =>
3)

))


(unittest "scope" (
((let ()
(define test
    (let ((x 0))
      (lambda (y)
        (if (> y x)
            (set! x y)
            (set! x 0))
        x)))
(unittest "scope - " (
((test 1)
; => 
1)
((test 2)
; => 
2)
((test 1)
; => 
0)))))

((let ()
(define a 10)
(define f (lambda () a))
(define g (lambda () (let ((a 20)) (f))))
(define h (lambda () (let ((a 30)) (f))))
(unittest "lexical-scoping" (
((g) 
; => 
10) ; 動的スコープだと 20 が返る
((h)
; => 
10) ; 動的スコープだと 30 が返る
))))

))


