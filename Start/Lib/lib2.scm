; atom?
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

; cxxxxr
(define cadddr (lambda (xs) (car (cdddr xs))))
(define caaddr (lambda (xs) (car (caddr xs))))
(define cadadr (lambda (xs) (car (cdadr xs))))

;(define (print e) 
;  (display e) (newline)
;  e)

(define (assoc thing alist)
   (if (null? alist)
       #f
       (if (equal? (car (car alist)) thing)
           (car alist)
           (assoc thing (cdr alist)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (square x) (* x x))
(define fib (lambda (n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2)))))))

(define (average x y)
  (/ (+ x y) 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 文字 str を n 回繰り返して作成した文字列を返す関数
(define (make-string n str)
  (if (eq? n 0)
      ""
      (++ str (make-string (- n 1) str))))

