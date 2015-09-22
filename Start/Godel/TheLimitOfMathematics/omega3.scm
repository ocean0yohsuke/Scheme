(load "Lib/lib.scm")
(load "Lib/lib2.scm")

; X* = bits X
; H(x) denotes the size in bits of the smallest program that makes our standard universal Turing machine compute x.

;   I prove that Omega is irreducible algorithmic information.
; The precise result turns out to be this:
;     H(Omega(N)) > N - c.
; This inequality states that to get the first N bits of the halting probability,
; you need a program that's more than N — c bits long. And the
; reason is that if you knew the first N bits of the halting probability,
; that would enable you to solve the halting problem for all programs up
; to N bits in size. That's how you prove that Omega is irreducible.

; Show that
;   H(Omega(n)) > n - 6840.
; Omega(n) is the first n bits of Omega,
; where we choose
;   Omega = xxxO111111...
; instead of
;   Omega = xxx1OOOOOO...
; if necessary.

; Here is the self-delimiting Universal Turing Machine!
(define (U p) (cadr (try 'no-time-limit '(eval (read-exp)) p)))
; => U

; Here is the prefix, a program that solves the halting problem for all programs up to N bits in size.
(define pi 
 '(let ((omega (lambda (t)
    (define (count-halt prefix bits-left-to-extend)
      (if (eq? bits-left-to-extend 0)
        (if (eq? 'success (car (try 1 '(eval (read-exp)) prefix))) 1 0)
        (+ (count-halt (append prefix '(0)) (- bits-left-to-extend 1)) 
           (count-halt (append prefix '(1)) (- bits-left-to-extend 1)))))
    (cons (count-halt nil t) (cons / (cons (^ 2 t) nil))))))

  (define (<=rat x y) ; compare two rationals
    (<= (* (car x) (caddr y)) (* (caddr x) (car y))))

  (let* ((w (eval (read-exp)))
         (n (length w))
         (w (cons (base2-to-10 w) (cons / (cons (^ 2 n) nil)))))
  (define (loop t)
    (define (big prefix bits-left-to-add)
      (if (eq? 0 bits-left-to-add)
        (cons (cadr (try (+ t 1) '(eval (read-exp)) prefix)) nil)
        (append (big (append prefix '(0)) (- bits-left-to-add 1))
                (big (append prefix '(1)) (- bits-left-to-add 1)))))
    (if (<=rat w (omega t))
        (big nil n)
        (loop (+ t 1))))

  (loop 0))))
; => pi

; Run pi.
(U (append (bits pi)
           ; Omega(8): Toy program to compute first 8 bits of Omega
           (bits ''(0 0 0 0 0 0 0 1))))
; => (out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data 
;  () out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data
;  out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data
;  out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data
;  out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data
;  out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data
;  out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data
;  out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data
;  out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data
;  out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data
;  out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data
;  out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data
;  out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data
;  out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data
;  out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data
;  out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data
;  out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data
;  out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data
;  out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data
;  out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data
;  out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data
;  out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data
;  out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data
;  out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data
;  out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data out-of-data
;  out-of-data out-of-data out-of-data out-of-data out-of-data)

;   This program, (pi Omega(N)*) for U, cannot itself have program-size complexity < N, because it
; can't be contained within itself.
; So it can't be the output of a program less than or equal to N bits in size, 
; it's got to have program-size complexity greater than N.
; Therefore this program 
;       pi Omega(N)*
; for producing the list
;       U(pi Omega(N)*)
; must be greater than N bits in size. In other words, (the size of any
; program that calculates the first N bits of Omega) plus |pi| has got to be
; greater than N, So you get this inequality
;       H(Omega(N)) + |pi| > N
; where |pi| = (length (bits pi))

; |pi|
(length (bits pi))
; => 6712

; H(Omega(8))
(length (bits ''(0 0 0 0 0 0 0 1)))
; => 152

(length (bits (U (append (bits pi)
                         (bits ''(0 0 0 0 0 0 0 1))))))
; => 24520

