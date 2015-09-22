(load "Lib/lib.scm")

(newline)
(display "decimal <--> binary conversions") (newline)
(base10-to-2 255)
; => (1 1 1 1 1 1 1 1)
(base10-to-2 256)
; => (1 0 0 0 0 0 0 0 0)
(base10-to-2 257)
; => (1 0 0 0 0 0 0 0 1)
(base2-to-10 '(1 1 1 1))
; => 15
(base2-to-10 '(1 0 0 0 0))
; => 16
(base2-to-10 '(1 0 0 0 1))
; => 17

(newline)
(display "illustrate eval & try") (newline)
(display "--- eval ---") (newline)
(eval (print '(+ (print 5) (print 15))))
(+ (print 5) (print 15))
; 5
; 15
; => 20
(display "--- try ---") (newline)
(try 0 (print '(+ (print 5) (print 15))) nil)
; (+ (print 5) (print 15))
; 5
; 15
; => (success 20 ())
(try 0 (print '(+ (printC 5) (printC 15))) nil)
; (+ (printC 5) (printC 15))
; 5
; 15
; => (success 20 (5 15))


(newline)
(display "eval & try use initial variable bindings") (newline)
(define x '(d e f))
; => x
(cons x ())
; => ((d e f))
(eval '(cons x nil))
; => ((d e f))
(try 0 '(cons x nil) nil)
; => (success ((d e f)) ())

(newline)
(display "to illustrate time limits") (newline)
(define five!
  '(letrec ((try-loop (lambda (x)
              (if (= (capture x) 0)
                  1
                  (* x (try-loop (- x 1)))))))
     (try-loop 5)))
; => five!
(try 'no-time-limit five! nil)
; => (success 120 (5 4 3 2 1 0))

(newline)
(display "time limit is nesting depth of re-evaluations due to function calls & eval & try") (newline)
(try 0 five! nil)
; => (failure out-of-time ())
(try 1 five! nil)
; => (failure out-of-time (5))
(try 2 five! nil)
; => (failure out-of-time (5 4))
(try 3 five! nil)
; => (failure out-of-time (5 4 3))
(try 4 five! nil)
; => (failure out-of-time (5 4 3 2))
(try 5 five! nil)
; => (failure out-of-time (5 4 3 2 1))
(try 6 five! nil)
; => ((success 120 (5 4 3 2 1 0)))
(try 7 five! nil)
; => ((success 120 (5 4 3 2 1 0)))
(try 8 five! nil)
; => ((success 120 (5 4 3 2 1 0)))


(newline)
(display "to illustrate running out of data") (newline)
(define two*
  '(letrec ((try-loop (lambda (x)  
              (if (= 0 x) 
                  nil
                  (cons (* 2 (capture (read-bit))) (try-loop (- x 1)))))))
     (try-loop 5)))
; => two*
(try 5 two* '(1 0 1 0 1))
; => (failure out-of-time (1 0 1 0 1))
(try 6 two* '(1 0 1 0 1))
; => (success (2 0 2 0 2) (1 0 1 0 1))
(try 7 two* '(1 0 1 0 1))
; => (success (2 0 2 0 2) (1 0 1 0 1))
(try 6 two* '(1 0 1))
; => (failure out-of-data (1 0 1))
(try 'no-time-limit two* '(1 0 1))
; => (failure out-of-data (1 0 1))

(newline)
(display "bits") (newline)
(bits 'a)
; => (0 1 1 0 0 0 0 1 0 0 0 0 1 0 1 0)
(try 17 
  '(letrec ((try-loop (lambda (x)  
              (if (= 0 x) 
                  nil
                  (cons (* 2 (capture (read-bit))) (try-loop (- x 1)))))))
     (try-loop 16))
  (bits 'a))
; => (success (0 2 2 0 0 0 0 2 0 0 0 0 2 0 2 0) (0 1 1 0 0 0 0 1 0 0 0 0 1 0 1 0))

(newline)
(display "illustrate nested try's") (newline)
(display "most constraining limit wins") (newline)
(try 10
  ; infinite loop
  '(letrec ((try-loop (lambda (n)
              (let ((a (capture (+ n 1))))
                (try-loop a)))))
     (try-loop 0))
  nil)
; => (failure out-of-time (1 2 3 4 5 6 7 8 9 10))
(try 20
  '(cons 'abcdef (try 10
                   ; infinite loop
                   '(letrec ((try-loop (lambda (n)
                               (let ((a (capture (+ n 1))))
                                 (try-loop a)))))
                      (try-loop 0))
                   nil)) 
  nil)
; => (success (abcdef failure out-of-time (1 2 3 4 5 6 7 8 9 10)) ())
(try 10
  '(cons 'abcdef (try 20
                   ; infinite loop
                   '(letrec ((try-loop (lambda (n)
                               (let ((a (capture (+ n 1))))
                                 (try-loop a)))))
                      (try-loop 0))
                   nil)) 
  nil)
; => (failure out-of-time ())



