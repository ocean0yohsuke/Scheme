(load "Lib/lib.scm")
(load "Lib/lib2.scm")

; Show that a formal system of complexity N
; can't determine more than 
; N + 6870 + 7600 bits of Omega.
;
; Formal system is a never halting lisp expression
; that outputs lists of the form (1 0 X 0 X X X X 1 0).
; This stands for the fractional part of Omega,
; and means that these 0,1 bits of Omega are known.
; X stands for an unknown bit.
 
; Count number of bits in an omega that are determined.
(define (number-of-bits-determined w)
    (if (null? w) 0
      (+ (number-of-bits-determined (cdr w))
         (if (eq? 'X (car w)) 0 1))))
; Test it.
(number-of-bits-determined '(X X X))
; => 0
(number-of-bits-determined '(1 X X))
; => 1
(number-of-bits-determined '(1 X 0))
; => 2
(number-of-bits-determined '(1 1 0))
; => 3
 
(newline)

; Merge bits of data into unknown bits of an omega.
(define (supply-missing-bits w)
  (if (null? w) nil
    (cons (if (eq? 'X (car w))
              (read-bit)
              (car w))
          (supply-missing-bits (cdr w)))))
; Test it.
(cadr (try 'no-time-limit '(supply-missing-bits '(0 0 X 0 0 X 0 0 X)) '(1 1 1)))
; => (0 0 1 0 0 1 0 0 1)
(cadr (try 'no-time-limit '(supply-missing-bits '(1 1 X 1 1 X 1 1 1)) '(0 0)))
; => (1 1 0 1 1 0 1 1 1)

(newline)

; Examine omegas in list w to see if in any one of them
; the number of bits that are determined is greater than n.
; Returns false to indicate not found, or what it found.
(define (examine w n)
  (if (null? w) #f 
  (if (< n (number-of-bits-determined (car w)))
      (car w)
      (examine (cdr w) n))))
; Test it.
(examine '((1 1) (1 1 1)) 0)
; => (1 1)
(examine '((1 1) (1 1 1)) 1)
; => (1 1)
(examine '((1 1) (1 1 1)) 2)
; => (1 1 1)
(examine '((1 1) (1 1 1)) 3)
; => #f
(examine '((1 1) (1 1 1)) 4)
; => #f

(newline)

; This is an identity function with the size-effect of
; displaying the number of bits in a binary string.
(define (display-number-of-bits string)
  (begin
    (print (length string))
    string))

(newline)

; Universal Turing machine U
(define (U p) (cadr (try 'no-time-limit '(eval (read-exp)) p)))
(U 
 (append ; Append missing bits of Omega to rest of program.
 ; Display number of bits in entire program excepting the missing bits of Omega.
 (display-number-of-bits
 (append ; Append prefix and formal axiomatic system.
 ; Display number of bits in the prefix.
 (display-number-of-bits (bits (quote
 
 ; Count number of bits in an omega that are determined.
 (letrec ((number-of-bits-determined (lambda (w)
   (if (null? w) 0
     (+ (number-of-bits-determined (cdr w))
        (if (eq? 'X (car w)) 0 1))))))
 ; Merge bits of data into unknown bits of an omega.
 (define (supply-missing-bits w)
   (if (null? w) nil
     (cons (if (eq? 'X (car w))
               (read-bit)
               (car w))
           (supply-missing-bits (cdr w)))))
 
 ; Examine omegas in list w to see if in any one of them
 ; the number of bits that are determined is greater than n.
 ; Return false to indicate not found, or what it found.
 (define (examine w n)
   (if (null? w) #f 
   ; (if (< n (number-of-bits-determined (car w)))
   (if (< 1 ; Change n to 1 here so will succeed.
          (number-of-bits-determined (car w)))
       (car w)
       (examine (cdr w) n))))

 ; Main Loop - t is time limit, fas is bits of formal axiomatic system read so far.
 (define (loop t fas)
   (let* ((v (try t '(eval (read-exp)) fas))
          ; Look for theorem which determines more than
          ;    (c + fas of bits read + size of this prefix)
          ; bits of Omega. Here c = 9488 is the constant in the inequality
          ;    H(Omega(n)) > n - c (see omega3.scm)
          ; where n = c + fas of bits read + size of this prefix 
          ; If this prefix returned (1 1 0), H(Omega(n)) is the size of this prefix.
          ; Then 
          ;    size of this prefix > c + fas of bits read + size of this prefix - c
          ; i.e.
          ;    0 > fas of bits read
          ; Contradiction!
          (theorems (caddr v))
          (s (examine theorems (+ 9488 (+ (length fas) 6912)))))
   (if s (supply-missing-bits s)                 ; Found it! Merge in undetermined bits, output result, and halt. Contradiction!
   (if (eq? (car v) 'success) 'failure           ; Surprise, formal system halts, so we do too.
   (if (eq? (cadr v) 'out-of-data) 
     (loop t (append fas (cons (read-bit) nil))) ; Read another theorem of the formal axiomatic system.
   (if (eq? (cadr v) 'out-of-time) 
     (loop (+ t 1) fas)                          ; Increase time limit.
   'unexpected-condition ; This should never happen.
 ))))))

 (loop 0 nil) ; Initially, 0 time limit and no bits of formal axiomatic system read.
 
 )))) ; end of prefix, start of formal axiomatic system
 
 ; Toy formal system with only one theorem.]
 (bits '(printC '(1 X 0)))
 
 )) ; end of prefix and formal axiomatic system
 
 '(1) ; Missing bit of Omega that is needed.
))
; 6032
; 6176
; (1 X 0)
; => (1 1 0)



