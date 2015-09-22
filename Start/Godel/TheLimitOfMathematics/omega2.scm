; Omega in the limit from below!
; Version II.
(load "Lib/lib.scm")
(load "Lib/lib2.scm")

; Count programs with prefix bit string p that halt within time t
; among all possible extensions by e more bits.
(define (count-halt prefix time bits-left-to-extend)
  (if (eq? bits-left-to-extend 0)
    (let ((v (try time '(eval (read-exp)) prefix)))
      (if (eq? 'success (car v)) 
          1 
          0))
    (+ (count-halt (append prefix '(0)) time (- bits-left-to-extend 1)) 
       (count-halt (append prefix '(1)) time (- bits-left-to-extend 1)))))
; => count-halt
(count-halt (bits '(cons (read-bit) (cons (read-bit) nil))) 'no-time-limit 0)
; => 0
(count-halt (bits '(cons (read-bit) (cons (read-bit) nil))) 'no-time-limit 1)
; => 0
(count-halt (bits '(cons (read-bit) (cons (read-bit) nil))) 'no-time-limit 2)
; => 4
(count-halt (bits '(cons (read-bit) (cons (read-bit) nil))) 'no-time-limit 3)
; => 8

; Omega in the limit from below!
(define (omega t)
  (cons (count-halt nil t t) 
        (cons / (cons (^ 2 t) nil))))
; => omega
(omega 0)
; => (0 </> 1)
(omega 1)
; => (0 </> 2)
(omega 2)
; => (0 </> 4)
(omega 3)
; => (0 </> 8)

; Only the newline character '\n' successes
(omega 8)
; => (1 </> 256) 

(omega 9)
; => (2 </> 512)
;(omega 10)
; => (4 </> 1024)
;(omega 11)
; => (8 </> 2048)
;(omega 12)
; => (16 </> 4096)

(newline)

(stacktracelength 0)

; fast version
(set! omega (lambda (t)
  (define count-halt (lambda (prefix bits-left-to-extend)
    (let ((x (try t '(eval (read-exp)) prefix)))
      (if (eq? 'success (car x)) (^ 2 bits-left-to-extend)
      (if (eq? 'out-of-time (cadr x)) 0
      (if (eq? bits-left-to-extend 0) 0
      (+ (count-halt (append prefix '(0)) (- bits-left-to-extend 1))
         (count-halt (append prefix '(1)) (- bits-left-to-extend 1)))))))))
  (cons (count-halt nil t) 
        (cons / (cons (^ 2 t) nil)))))
; => (\(t) -> (define count-halt (lambda (prefix bits-left-to-extend) (let ((x (try t '(eval (read-exp)) prefix))) (if (eq? 'success (car x)) (^ 2 bits-left-to-extend) (if (eq? 'out-of-time (cadr x)) 0 (if (eq? bits-left-to-extend 0) 0 (+ (count-halt (append prefix '(0)) (- bits-left-to-extend 1)) (count-halt (append prefix '(1)) (- bits-left-to-extend 1))))))))) >> (cons (count-halt nil t) (cons / (cons (^ 2 t) nil))))
(omega 0)
(omega 1)
(omega 2)
(omega 3)
(omega 8)
(omega 9)
;(omega 10)
;(omega 11)
; => (0 </> 1)
; => (0 </> 2)
; => (0 </> 4)
; => (0 </> 8)
; => (1 </> 256)
; => (2 </> 512)
; => (4 </> 1024)
; => (8 </> 2048)

