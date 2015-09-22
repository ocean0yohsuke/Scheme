; http://kreisel.fam.cx/webmaster/clog/img/www.ice.nuie.nagoya-u.ac.jp/~h003149b/lang/actor/actor.html
(load "Lib/lib.scm")

(define _fact
  (lambda (k n)
    (if (= n 0)
        (k 1)
        (_fact (lambda (u)
                 (k (* n u)))
               (- n 1)))))
  
(_fact (lambda (x) x) 10)
; => 3628800

(define _+
  (lambda (x y k)
    (k (+ x y))))
(define _-
  (lambda (x y k)
    (k (- x y))))
(define _*
  (lambda (x y k)
    (k (* x y))))
(define _=
  (lambda (x y k)
    (k (= x y))))
(set! _fact
  (lambda (n k)
    (_= n 0
      (lambda (u)
        (if u
            (k 1)
            (_- n 1
              (lambda (v)
                (_fact v
                       (lambda (w)
                         (_* n w k))))))))))
(_fact 10 (lambda (x) x))
; => 3628800

