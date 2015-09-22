(load "Lib/lib.scm")

(unittest "begin" (
((begin 1 2 3 4 5)
; => 
5)
))

(define cond-test
  (lambda (x)
    (cond ((eq? x 'a) 1)
          ((eq? x 'b) 2)
          ((eq? x 'c) 3)
          (else       0))))
(unittest "cond" (
((cond-test 'a) 1)
((cond-test 'b) 2)
((cond-test 'c) 3)
((cond-test 'd) 0)
))

(define case-test
  (lambda (x)
    (case x
      ((a b c) 1)
      ((d e f) 2)
      ((g h i) 3)
      (else    0))))
(unittest "case" (
((case-test 'a) 1)
((case-test 'e) 2)
((case-test 'i) 3)
((case-test 'j) 0)
))

(define fact-do (lambda (x)
       (do ((n 1 (+ n 1))
            (result 1 (* result n)))                   
           ((> n x) result))))
(unittest "fact-do" (
((fact-do 10) 3628800)
))

(unittest "evalmacro" (
[(evalmacro '(begin 1 2 3 4 5))
; => 
((lambda () 1 2 3 4 5))]

[(evalmacro '(lambda (x)
  (cond ((eq? x 'a) 1)
        ((eq? x 'b) 2)
        ((eq? x 'c) 3)
        (else       0))))
; => 
(lambda (x) (if (eq? x 'a) ((lambda () 1)) (if (eq? x 'b) ((lambda () 2)) (if (eq? x 'c) ((lambda () 3)) ((lambda () 0))))))]

[(evalmacro '(letrec ((fibo (lambda (k)
      (cond ((= k 0) 0)
            ((= k 1) 1)
            (else (+ (fibo (- k 1))
                     (fibo (- k 2))))))))
 (fibo 10)))
; => 
(letrec ((fibo (lambda (k) (cond ((= k 0) 0) ((= k 1) 1) (else (+ (fibo (- k 1)) (fibo (- k 2)))))))) (fibo 10))]
[(letrec ((fibo (lambda (k) (cond ((= k 0) 0) ((= k 1) 1) (else (+ (fibo (- k 1)) (fibo (- k 2)))))))) (fibo 10))
; => 
55]

))



