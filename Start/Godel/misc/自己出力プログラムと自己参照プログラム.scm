; http://kreisel.fam.cx/webmaster/clog/img/www.ice.nuie.nagoya-u.ac.jp/~h003149b/lang/quine.html
(load "Lib/lib.scm")

(newline)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(display "自己出力プログラム")(newline)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
((lambda (x) (list x (list 'quote x)))
   '(lambda (x) (list x (list 'quote x))))
; => ((lambda (x) (list x (list 'quote x))) '(lambda (x) (list x (list 'quote x))))

((lambda (x) `(,x ',x)) '(lambda (x) `(,x ',x)))
; => ((lambda (x) `(,x ',x)) '(lambda (x) `(,x ',x)))


(newline)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(display "自己参照プログラム")(newline)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

((lambda (x y) `(,y (,x ',x ',y))) '(lambda (x y) `(,y (,x ',x ',y))))
; => (\(y) -> `(,y (,x ',x ',y)))

((lambda (x y) `(,y (,x ',x ',y))) '(lambda (x y) `(,y (,x ',x ',y)))
'f)
; => (f ((lambda (x y) `(,y (,x ',x ',y))) '(lambda (x y) `(,y (,x ',x ',y))) 'f))


(define (flatten xs)
     (if (pair? xs)
         (if (pair? (car xs))
             (append (flatten (car xs)) (flatten (cdr xs)))
                     (cons (car xs) (flatten (cdr xs))))
         xs))
(flatten
 ((lambda (x y) `(,y (,x ',x ',y))) '(lambda (x y) `(,y (,x ',x ',y)))
  '(define (flatten xs)
     (if (pair? xs)
         (if (pair? (car xs))
             (append (flatten (car xs)) (flatten (cdr xs)))
                     (cons (car xs) (flatten (cdr xs))))
         xs))))
; => (define flatten xs if pair? xs if pair? car xs append flatten car xs flatten cdr xs cons car xs flatten cdr xs xs lambda x y quasiquote unquote y unquote x quote unquote x quote unquote y quote lambda x y quasiquote unquote y unquote x quote unquote x quote unquote y quote define flatten xs if pair? xs if pair? car xs append flatten car xs flatten cdr xs cons car xs flatten cdr xs xs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(display "
補足3 - クワイン化について
")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define quining (lambda (x) `(,x ',x)))
(quining 'quining)
; => (quining 'quining)

