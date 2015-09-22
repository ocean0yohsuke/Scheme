(load "Lib/lib.scm")
;(define Y
;  (lambda (l)
;    ((lambda (s) (l (lambda (x) ((s s) x))))
;     (lambda (s) (l (lambda (x) ((s s) x)))))))
;(define Y
;  (lambda (g)
;    ((lambda (s) (g (lambda x (apply (s s) x))))
;     (lambda (s) (g (lambda x (apply (s s) x)))))))

(unittest "Y" (
(((Y (lambda (fact)
       (lambda (n)
         (if (= n 0)
             1
             (* n (fact (- n 1))))))) 10)
  3628800)

((((Y (lambda (iter)
   (lambda (n) (lambda (a) 
            (if (zero? n)
                a
                ((iter (- n 1)) (* a n))))))) 10) 1)
  3628800)
))



(unittest "let - Y" (
((let ((fact (Y (lambda (fact)
     (lambda (n)
       (if (= n 0)
           1
           (* n (fact (- n 1)))))))))
   (fact 10))
  3628800)

((let ((iter (Y (lambda (iter)
   (lambda (n) (lambda (a) 
            (if (zero? n)
                a
                ((iter (- n 1)) (* a n)))))))))
   ((iter 10) 1))
  3628800)
))


(unittest "letrec" (
((letrec ((fact
     (lambda (n)
       (if (= n 0)
           1
           (* n (fact (- n 1)))))))
   (fact 10))
  3628800)

((letrec ((iter (lambda (n a)
            (if (zero? n)
                a
                (iter (- n 1) (* a n))))))
   (iter 10 1))
  3628800)
((letrec ((iter (lambda (n) (lambda (a)
            (if (zero? n)
                a
                ((iter (- n 1)) (* a n)))))))
   ((iter 10) 1))
  3628800)
))


(unittest "a . args" (
((let ((f (lambda (a . args) (cons a args)))) (f 1 2))
 (1 2))
((let ((f (Y (lambda (f) (lambda (a . args) (cons a args)))))) (f 1 2))
 (1 2))
((letrec ((f (lambda (a . args) (cons a args)))) (f 1 2))
 (1 2))


((let ((f (Y (lambda (f) (lambda args args))))) (f 1 2))
 (1 2))
((letrec ((f (lambda args args))) (f 1 2))
 (1 2))

))

(unittest "letrec: mutual recursive; PQ" (
((let ((E1 (lambda (female male) (lambda (n) (if (= n 0) 1 (- n (male (female (- n 1))))))))
      (E2 (lambda (female male) (lambda (n) (if (= n 0) 0 (- n (female (male (- n 1)))))))))
  (let ((female (P E1 E2))
        (male   (Q E1 E2)))
    (list (female 10) (male 10))))
; => 
(6 6))
((letrec ((female (lambda (n) (if (= n 0) 1 (- n (male (female (- n 1)))))))
	     (male (lambda (n) (if (= n 0) 0 (- n (female (male (- n 1))))))))
  (list (female 10) (male 10)))
; => 
(6 6))

((letrec ((myodd? (lambda (n) (if (= n 0) #f (myeven? (- n 1)))))
	     (myeven? (lambda (n) (if (= n 0) #t (myodd? (- n 1))))))
  (myodd? 2))
; => 
#f)

))


