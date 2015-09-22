(load "Lib/lib.scm")

(unittest "not, null?, zero?, positive?, negative?, even?, odd?" (
  ((not #t) #f)
  ((not #f) #t)
  ((null? '()) #t)
  ((null? '(1)) #f)
  ((zero? 0) #t)
  ((zero? 1) #f)
  ((zero? 0.0) #t)
  ((zero? 1.0) #f)
  ((positive? 1) #t)
  ((positive? 0) #f)
  ((negative? -10) #t)
  ((negative? 0) #f)
  ((negative? 10) #f)
  ((even? 1) #f)
  ((even? 2) #t)
  ((even? 0) #t)
  ((odd? 0) #f)
  ((odd? 101) #t)
))

(unittest "abs, max, min, gcd, lcm" (
  ((abs -10) 10)
  ((abs -10.1) 10.1)
  ((abs 10.1) 10.1)
  ((abs 10) 10)
  ((max 1 2) 2)
  ((min 1 2) 1)
  ((max 1 2 3 4 5 6 7) 7)
  ((min 1 2 3 4 5 6 7) 1)
  ((min 1) 1)
  ((max 1) 1)
  ((gcd 24 12) 12)
  ((gcd 12 128) 4)
  ((gcd 24 12 128) 4)
  ((gcd) 0)
  ((lcm 7 5) 35)
  ((lcm 7 5 9) 315)
  ((lcm) 1)
))

(unittest "list, append, length, reverse" (
  ((list 1 2 3 4 5) (1 2 3 4 5))
  ((list) ())
  ((append '(a b c) '(d e f))            (a b c d e f))
  ((append '(a b c) '(d e f) '(1 2 3 4)) (a b c d e f 1 2 3 4))
  ((append)          ())
  ((append '(a b c)) (a b c))
  ((length '()) 0)
  ((length '(a b c d e)) 5)
  ((reverse '())          ())
  ((reverse '(a b c d e)) (e d c b a))
))

(unittest "list-tail, list-ref" (
  ((list-tail '(a b c d e) 0) (a b c d e))
  ((list-tail '(a b c d e) 1) (b c d e))
  ((list-tail '(a b c d e) 4) (e))
  ((list-tail '(a b c d e) 5) ())
  ((list-ref '(a b c d e) 0) a)
  ((list-ref '(a b c d e) 3) d)
  ((list-ref '(a b c d e) 4) e)
))

(unittest "memq, memv, member, find" (
  ((memq 'c '(a b c d e)) (c d e))
  ((memq 'f '(a b c d e)) #f)
  ((memv 'c '(a b c d e)) (c d e))
  ((memv 'f '(a b c d e)) #f)
  ((memv '(c d) '((a b) (c d) (e f))) 
   ((c d) (e f)))
  ((member '(c d) '((a b) (c d) (e f))) 
   ((c d) (e f)))
  ((member '(c e) '((a b) (c d) (e f))) 
   #f)
  ((find even? '(1 3 5 6 7)) 
   6)
  ((find odd? '(1 3 5 6 7)) 
   1)
))

(unittest "map" (
  ((map (lambda (x) (* x x)) '(1 2 3 4 5)) 
   (1 4 9 16 25))
  ((map (lambda (x y) (* x y)) '(1 2 3 4 5) '(6 7 8 9 10))
   (6 14 24 36 50))
  ((map (lambda (x y) (cons x y)) '(1 2 3 4 5) '(6 7 8 9 10))
   ((1 . 6) (2 . 7) (3 . 8) (4 . 9) (5 . 10)))
))

(unittest "filter" (
  ((filter (lambda (x) (even? x)) '(1 2 3 4 5 6 7 8))
   (2 4 6 8))
  ((filter (lambda (x) (odd? x)) '(1 2 3 4 5 6 7 8))
   (1 3 5 7))
))

(unittest "fold-left, fold-right" (
  ((fold-left + 0 '(1 2 3 4 5 6 7 8 9 10))
   55)
  ((fold-left (lambda (a x) (cons x a))
              '() 
              '(1 2 3 4 5 6 7 8 9 10))
   (10 9 8 7 6 5 4 3 2 1))

  ((fold-right + 0 '(1 2 3 4 5 6 7 8 9 10))
   55)
  ((fold-right cons 
               '() 
               '(1 2 3 4 5 6 7 8 9 10))
   (1 2 3 4 5 6 7 8 9 10))
))

(unittest "any, every" (
  ((any even? '(1 3 5 6 7 9)) #t)
  ((any even? '(1 3 5 7 9)) #f)
  ((any (lambda (x y) (< x y)) '(1 3 5 7 9) '(2 4 6 8 10)) #t)
  ((any (lambda (x y) (< x y)) '(2 4 6 8 10) '(1 3 5 7 9)) #f)
  
  ((every even? '(2 4 6 8 10)) #t)
  ((every even? '(2 4 6 8 11)) #f)
  ((every (lambda (x y) (< x y)) '(1 3 5 7 9) '(2 4 6 8 10)) #t)
  ((every (lambda (x y) (< x y)) '(1 3 5 7 9) '(2 4 6 8 0)) #f)
))

(unittest "and, or" (
  ((and 1 2 3 4 5)  5)
  ((and 1 2 #f 4 5) #f)

  ((or 1 2 3)    1)
  ((or #f 2 3)   2)
  ((or #f #f 3)  3)
  ((or #f #f #f) #f)
))

(unittest "let" (
((let ((a 10)
       (b 20))
(let ((a 0) 
      (b 1)) 
(list a b)))
(0 1))
))

; 変数を定義する際、前から順に定義する
(unittest "let*" (
  ((let* ((a 1) 
          (b (+ a 1))) 
     (list a b))
   (1 2))
  ((let* ((a 1) 
          (b (+ a 1)) 
          (c (* b 2))) 
     (list a b c))
   (1 2 4))
))

(unittest "letrec" (
  ((let ((fact (lambda (x)
     (letrec ((iter (lambda (n a)
                (if (zero? n)
                    a
                    (iter (- n 1) (* a n))))))
       (iter x 1)))))
     (fact 10))
    3628800)
))

(unittest "begin" (
  ((begin 1 2 3 4 5) 5)
  ((let ((a' 10))
     (begin 1 2 3 4 a'))
   10)
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

(unittest "do" (
  ((let ((fact-do (lambda (x)
       (do ((n 1 (+ n 1))
            (result 1 (* result n)))                   
           ((> n x) result)))))
     (fact-do 10))
   3628800)

  ((let ((fact-do (lambda (x)
       (do ((n 1 (+ n 1))
            (result 1))                   
           ((> n x) result)
           (set! result (* result n))))))
     (fact-do 10))
   3628800)
))

(load "Lib/set.scm")
(unittest "set" (
  ( (member? 1 '(1 2 3)) #t )
  ( (member? 4 '(1 2 3)) #f )

  ( (subset? '(1 2) '(1 2 3)) #t )
  ( (subset? '(1 4) '(1 2 3)) #f )

  ( (union '(1 2 3) '(2 3 4))     (1 2 3 4) )
  ( (unionl '((1 2) (2 3) (3 4))) (1 2 3 4) )

  ( (intersection '(1 2 3) '(2 3 4)) (2 3) )

  ( (complement '(1 2 3) '(2 3 4)) (1) )

  ( (product1 3 '(4 5 6))             ((3 4) (3 5) (3 6)))
  ( (product '(1 2 3) '(x y z))       ((1 x) (1 y) (1 z) (2 x) (2 y) (2 z) (3 x) (3 y) (3 z)))
  ( (product2 3 '((4 5) (5 6) (6 7))) ((3 4 5) (3 5 6) (3 6 7)))

  ( (subsets '(1 2 3))   (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)) )
  ( (length (subsets '(1 2 3))) 8)

  ( (subsets '(1 2 3 4)) (() (4) (3) (3 4) (2) (2 4) (2 3) (2 3 4) (1) (1 4) (1 3) (1 3 4) (1 2) (1 2 4) (1 2 3) (1 2 3 4)) )
  ( (length (subsets '(1 2 3 4))) 16 )
))



