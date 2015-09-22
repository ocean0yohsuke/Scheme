; Omega in the limit from below!
(load "Lib/lib.scm")
(load "Lib/lib2.scm")

; Generate all bit strings of length k.
(define (all-bit-strings-of-size k)
  ; Append 0 and 1 to each element of list.
  (define (extend-by-one-bit x)
    (if (null? x) nil
    (cons (append (car x) '(0))
      (cons (append (car x) '(1))
        (extend-by-one-bit (cdr x))))))

  (if (eq? 0 k) '(())
    (extend-by-one-bit (all-bit-strings-of-size (- k 1)))))
; => all-bit-strings-of-size
(all-bit-strings-of-size 0)
; => (())
(all-bit-strings-of-size 1)
; => ((0) (1))
(all-bit-strings-of-size 2)
; => ((0 0) (0 1) (1 0) (1 1))
(all-bit-strings-of-size 3)
; => ((0 0 0) (0 0 1) (0 1 0) (0 1 1) (1 0 0) (1 0 1) (1 1 0) (1 1 1))
(all-bit-strings-of-size 4)
; => ((0 0 0 0) (0 0 0 1) (0 0 1 0) (0 0 1 1) (0 1 0 0) (0 1 0 1) (0 1 1 0) (0 1 1 1) (1 0 0 0) (1 0 0 1) (1 0 1 0) (1 0 1 1) (1 1 0 0) (1 1 0 1) (1 1 1 0) (1 1 1 1))

; Count programs in list p that halt within time t.
(define (count-halt p t)
  (if (null? p) 0
  (+ (if (eq? 'success (car (try t '(eval (read-exp)) (car p)))) 1 0)
     (count-halt (cdr p) t))))
; => count-halt
(count-halt (cons (bits '(+ 10 15)) nil) 99)
; => 1
(count-halt (cons (bits '(+ 10 15))
            (cons (bits '(define a 1)) nil))
 99)
; => 2
(count-halt (cons (append (bits '(read-bit)) '(1)) nil) 99)
; => 1
(count-halt (cons (append (bits '(read-exp)) '(1)) nil) 99)
; => 0
(count-halt (cons (append (bits '(read-bit)) '(1))
            (cons (append (bits '(read-exp)) '(1))
                  nil))
 99)
; => 1

; The kth lower bound on Omega
; is the number of k-bit strings that halt on U within time k
; divided by 2 raised to the power k.
(define (omega k) 
  (cons (count-halt (all-bit-strings-of-size k) k) 
        (cons / (cons (^ 2 k) nil))))
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


