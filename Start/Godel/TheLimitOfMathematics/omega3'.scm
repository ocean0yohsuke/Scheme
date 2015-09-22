(load "Lib/lib.scm")
(load "Lib/lib2.scm")

; Show that
;   H(Omega(n)) > n - 7552.
; Omega(n) is the first n bits of Omega,
; where we choose
;   Omega = xxxO111111...
; instead of
;   Omega = xxx1OOOOOO...
; if necessary.
 
; This is an identity function with the size-effect of
; displaying the length in bits of the binary prefix.
(define (display-length-of-prefix prefix)
  (begin
    (print (length prefix))
    prefix))

; Universal Turing machine U
(define (U p) (cadr (try 'no-time-limit '(eval (read-exp)) p)))

(U ; --- followed by its program.
 (append ; Append prefix and data.

 ; Code to display size of prefix in bits.
 (display-length-of-prefix (bits (quote

 (letrec ((count-halt (lambda (prefix time bits-left-to-extend)
   (if (eq? bits-left-to-extend 0)
     (if (eq? 'success (car (try time '(eval (read-exp)) prefix))) 1 0)
     (+ (count-halt (append prefix '(0)) time (- bits-left-to-extend 1)) 
        (count-halt (append prefix '(1)) time (- bits-left-to-extend 1)))))))
 (define (omega k)
   (cons (count-halt nil k k) (cons / (cons (^ 2 k) nil))))
 ; Compare two rational numbers, i.e., is x= (a / b) <= y= (c / d)?
 (define (<=rat x y)
   (let ((a (car x))
         (b (caddr x))
         (c (car y))
         (d (caddr y)))
     (<= (* a d) (* b c))))
 ; Union of all output of n-bit programs within time k.
 (define (big prefix time bits-left-to-add)
   (if (eq? 0 bits-left-to-add)
     (try time '(eval (read-exp)) prefix)
     (append (big (append prefix '(0)) time (- bits-left-to-add 1))
             (big (append prefix '(1)) time (- bits-left-to-add 1)))))

 ; Read and execute from remainder of tape
 ; a program to compute an n-bit
 ; initial piece of Omega.
 
 (let* ((w (print (eval (print (read-exp)))))
        ; Convert Omega to ratio of integers,
        ; i.e., from a bit string to a rational number.
        (n (length w))
        (w (cons (base2-to-10 w) (cons / (cons (^ 2 n) nil)))))
 
 (define (loop k)                          ; Main Loop ---
   (let ((x (print (omega (print k)))))    ; Compute the kth lower bound on Omega.
     (if (print (<=rat w x)) (big nil k n) ; Are the first n bits OK? If not, bump k and go loop.
         (loop (+ k 1)))))                 ; If so, form the union of all output of n-bit
                                           ; programs within time k, output it, and halt.
                                           ; All n-bit programs that ever halt halt by time k.
                                           ; Thus this union is bigger than anything of complexity
                                           ; less than or equal to n!
 
 ; This total output will be bigger than each individual output,
 ; and therefore must come from a program with more than n bits.
 ; Therefore this program itself must be more than n bits long.
 ; I.e., 7552 + H(Omega_n) > n. Q.E.D.
 
 ; Start main loop running with k = 0.
 (loop 0)
 )))))  ; end of display-length-of-prefix

 (bits (quote            ; Here is the data: an optimal program to compute n bits of Omega.
 '(0 0 0 0  0 0 0 1))))) ; n = 8! Are these really the first 8 bits of Omega?
; 7552
; '(0 0 0 0 0 0 0 1)
; (0 0 0 0 0 0 0 1)
; 0
; (0 </> 1)
; #f
; 1
; (0 </> 2)
; #f
; 2
; (0 </> 4)
; #f
; 3
; (0 </> 8)
; #f
; 4
; (0 </> 16)
; #f
; 5
; (0 </> 32)
; #f
; 6
; (0 </> 64)
; #f
; 7
; (0 </> 128)
; #f
; 8
; (1 </> 256)
; #t
; => (failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () success () () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data () failure out-of-data ())

