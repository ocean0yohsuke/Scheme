(load "Lib/lib.scm")

(newline)
(display "不動点オペレータY")(newline)
; 不動点オペレータについて

(define fact
    (lambda (n)
      (if (= n 0)
          1
          (* n (fact (- n 1))))))
(fact 10)
; => 3628800

(let ((fact (lambda (self n)
                (if (= n 0)
                1
                (* n (self self (- n 1)))))))
    (fact fact 10))
; => 3628800

(let* ((Y (lambda (g)
              ((lambda (s) (g (lambda (x) ((s s) x))))
               (lambda (s) (g (lambda (x) ((s s) x)))))))
         (fact (Y (lambda (f)
                    (lambda (n)
                      (if (= n 0)
                        1
                        (* n (f (- n 1)))))))))
    (fact 10))
; => 3628800

;(define Y
;    (lambda (g)
;      ((lambda (s) (g (lambda x (apply (s s) x))))
;       (lambda (s) (g (lambda x (apply (s s) x)))))))

(define my-gcd
    (Y (lambda (f)
         (lambda (a b)
           (if (= b 0)
               (abs a)
               (f b (mod a b)))))))

(my-gcd 123123 454545)
; => 273

(newline)
(display "相互再帰の場合")(newline)

(define female (lambda (n)
    (if (= n 0)
        1
        (- n (male (female (- n 1)))))))
  
(define male (lambda (n)
    (if (= n 0)
        0
        (- n (female (male (- n 1)))))))

(female 20)
; => 13
(male 20)
; => 12

;(define P
;    (lambda (g h)
;      ((lambda (s t)
;         (s s t))
;       (lambda (s t)
;         (g (lambda x (apply (s s t) x))
;            (lambda x (apply (t s t) x))))
;       (lambda (s t)
;         (h (lambda x (apply (s s t) x))
;            (lambda x (apply (t s t) x)))))))
;(define Q
;    (lambda (g h)
;      ((lambda (s t)
;         (t s t))
;       (lambda (s t)
;         (g (lambda x (apply (s s t) x))
;            (lambda x (apply (t s t) x))))
;       (lambda (s t)
;         (h (lambda x (apply (s s t) x))
;            (lambda x (apply (t s t) x)))))))

;(set! Y
;    (lambda (g)
;      ((lambda (s)
;         (s s))
;       (lambda (s)
;         (g (lambda x (apply (s s) x)))))))

(let ((E1 (lambda (f g)
            (lambda (n)
              (if (= n 0)
                  1
                  (- n (g (f (- n 1))))))))
      (E2 (lambda (f g)
            (lambda (n)
              (if (= n 0)
                  0
                  (- n (f (g (- n 1)))))))))
    (let ((female (P E1 E2))
          (male   (Q E1 E2)))
      (list (female 20) (male 20))))
; => (13 12)

