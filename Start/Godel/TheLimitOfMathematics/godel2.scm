(load "Lib/lib.scm")
(load "Lib/lib2.scm")

; Here is the self-delimiting Universal Turing Machine!
(define (U p) (cadr (try 'no-time-limit '(eval (read-exp)) p)))

; Show that a formal system of complexity N
; can't prove that a specific object has
; complexity > N + 4240.

; Postulation for reductio ad absurdum: 
; Formal system is a never halting lisp expression
; that output pairs 
;   (lisp object, lower bound on its complexity).
; E.g., (x 4) means that x has complexity H(x) greater than or equal to 4.

; Examine pairs to see if 2nd element is greater than lower bound.
; Returns false to indicate not found, or pair if found.
(define (examine pairs lower-bound)
  (if (null? pairs) #f
  (if (< lower-bound (cadr (car pairs))) (car pairs)
  (examine (cdr pairs) lower-bound))))
; => examine
(examine '((x 2) (y 3)) 0)
; => (x 2)
(examine '((x 2) (y 3)) 1)
; => (x 2)
(examine '((x 2) (y 3)) 2)
; => (y 3)
(examine '((x 2) (y 3)) 3)
; => #f
(examine '((x 2) (y 3)) 4)
; => #f

; Here is the prefix Pi, the program that outputs the theorems of the FAS
(define pi 
 '(letrec ((examine (lambda (pairs lower-bound)
    (if (null? pairs) #f
    (if (< lower-bound (cadr (car pairs)))
        (car pairs)
        (examine (cdr pairs) lower-bound))))))

  ; Main Loop - t is time limit, fas is bits of formal axiomatic system read so far.
  (define (loop t fas)                      ; Run formal axiomatic system again.
    (let* ((v (try t '(eval (read-exp)) fas))
           ; Look for theorem which is pair with 2nd element > fas of bits read + size of this prefix.
           (theorems (caddr v))
           (p (examine theorems (+ (length fas) 4072))))
    (if p (car p)                           ; Found it! Output first element of theorem and halt. Contradiction!
    (if (eq? (car v) 'success) 'failure     ; Surprise, formal system halts, so we do too.
    (if (eq? (cadr v) 'out-of-data) (loop t (append fas (cons (read-bit) nil)))
                                            ; Read another bit of the formal axiomatic system.
    (if (equal? (cadr v) 'out-of-time) (loop (+ t 1) fas)
                                            ; Increase time limit
    'unexpected-condition))))))

  (loop 0 nil) ; Initially, 0 time limit and no bits of formal axiomatic system read.
))
; => pi
; Size pi
(length (bits pi))
; => 4072

(newline)

; Size pi + fas
(length (append (bits pi) (bits '(printC '(xyz 9999)))))
; => 4240
; Here pi finds something suitable.
(U (append (bits pi) (bits '(printC '(xyz 4241)))))
; (xyz 4241)
; => xyz

(newline)

;   This contradicts the definition of elegance (lower bound on its complexity), because this
;      (4072 characters (formal axiomatic system))
; large LISP expression is at least one bit too small to produce
; that value.
;   So either the formal axiomatic system was lying, and produced a false theorem
; , or in fact this
;      (4072 characters (formal axiomatic system))
; won't work, because it will never prove that a specific object has complexity > N + 4240.

; Here pi doesn't find anything suitable.
(U (append (bits pi) (bits '(printC '(xyz 4240)))))
; (xyz 4240)
; => failure


