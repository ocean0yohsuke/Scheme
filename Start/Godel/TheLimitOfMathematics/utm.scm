(load "Lib/lib.scm")
(load "Lib/lib2.scm")

; H(x) denotes the size in bits of the smallest 
; program that makes our standard universal Turing machine compute x.

; |x|: the size (bits) of a binary string x

; First steps with my new construction for
; a self-delimiting Universal Turing Machine.
; We show that
; H(x,y) <= H(x) + H(y) + c
; and determine c.
; Consider a bit string x of length |x|.
; We also show that
; H(x) <= 2|x| + c
; and that
; H(x) <= |x| + H(the binary string for |x|) + c
; and determine both these c's.

; Here is the self-delimiting Universal Turing Machine!
; (with slightly funny handling of out-of-tape condition)
(define (U p) (cadr (try 'no-time-limit '(eval (read-exp)) p)))
; => U

(bits '(cons 'x (cons 'y (cons 'z nil))))
; => (0 0 1 0 1 0 0 0 0 1 1 0 0 0 1 1 0 1 1 0 1 1 1 1 0 1 1 0 1 1 1 0 0 1 1 1 0 0 1 1 0 0 1 0 0 0 0 0 0 1 1 1 1 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 1 0 0 0 0 1 1 0 0 0 1 1 0 1 1 0 1 1 1 1 0 1 1 0 1 1 1 0 0 1 1 1 0 0 1 1 0 0 1 0 0 0 0 0 0 1 1 1 1 0 0 1 0 0 1 0 0 0 0 0 0 0 1 0 1 0 0 0 0 1 1 0 0 0 1 1 0 1 1 0 1 1 1 1 0 1 1 0 1 1 1 0 0 1 1 1 0 0 1 1 0 0 1 0 0 0 0 0 0 1 1 1 1 0 1 0 0 0 1 0 0 0 0 0 0 1 1 0 1 1 1 0 0 1 1 0 1 0 0 1 0 1 1 0 1 1 0 0 0 0 1 0 1 0 0 1 0 0 1 0 1 0 0 1 0 0 1 0 1 0 0 1 0 0 0 0 1 0 1 0)
(U (bits '(cons 'x (cons 'y (cons 'z nil)))))
; => (x y z)
(U (append (bits '(cons 'a (print (read-exp)))) 
           (bits '(b c d))))
; (b c d)
; => (a b c d)

(newline)
(display "alpha") (newline)
; The length of alpha in bits is the
; constant c in H(x) <= 2|x| + 2 + c.
(define alpha 
  '(letrec ((loop (lambda () 
              (let ((x (read-bit))
                    (y (read-bit)))
              (if (equal? x y)
                  (cons x (loop))
                  nil)))))
     (loop)))
; => alpha
(length (bits alpha))
; => 904
(U (append (bits alpha) '(0 0 1 1 0 0 1 1 0 1)))
; => (0 1 0 1)
(U (append (bits alpha) '(0 0 1 1 0 0 1 1 0 0)))
; => out-of-data

(newline)
(display "beta") (newline)
; The length of beta in bits is the
; constant c in H(x,y) <= H(x) + H(y) + c.
(define beta 
  '(cons (eval (read-exp))
   (cons (eval (read-exp))
     nil)))
; => beta
(length (bits beta))
; => 432
(U
  (append
    (bits beta)
  (append
    (bits '(cons 'a (cons 'b (cons 'c nil))))
    (bits '(cons 'x (cons 'y (cons 'z nil)))))))
; => ((a b c) (x y z))
(U
  (append
    (bits beta)
  (append
    (append (bits alpha) '(0 0 1 1 0 0 1 1 0 1))
    (append (bits alpha) '(1 1 0 0 1 1 0 0 1 0)))))
; => ((0 1 0 1) (1 0 1 0))

(newline)
(display "gamma") (newline)
; The length of gamma in bits is the
; constant c in H(x) <= |x| + H(|x|) + c
(define gamma 
  '(letrec ((loop (lambda (k) 
             (if (= 0 k) nil
                 (cons (print (read-bit)) (loop (- k 1)))))))
     (loop (base2-to-10 (eval (print (read-exp)))))))
; => gamma
(length (bits gamma))
; => 1088
(U
  (append
    (bits gamma)
  (append
    ; Arbitrary program for U to compute number of bits
    (bits ''(1 0 0 0)) ; 8 -> k
    ; That many bits of data
    '(0 0 0 0 0 0 0 1))))
; '(1 0 0 0)
; 0
; 0
; 0
; 0
; 0
; 0
; 0
; 1
; => (0 0 0 0 0 0 0 1)

