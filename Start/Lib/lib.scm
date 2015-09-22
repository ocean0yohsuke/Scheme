; http://www.geocities.jp/m_hiroi/func/haskell37.html
;
; lib.scm : micro Scheme 用簡易ライブラリ
;
;           Copyright (C) 2013 Makoto Hiroi
;

(define nil ())
(define not (lambda (x) (if x #f #t)))
(define null? (lambda (x) (eq? x '())))
(define eqv? eq?)
(define list (lambda x x))

(define zero? (lambda (x) (= x 0)))
(define positive? (lambda (x) (< 0 x)))
(define negative? (lambda (x) (> 0 x)))
(define even? (lambda (x) (zero? (mod x 2))))
(define odd? (lambda (x) (not (even? x))))
(define abs (lambda (x) (if (negative? x) (- x) x)))
(define max
  (lambda (x . xs)
    (fold-left (lambda (a b) (if (< a b) b a)) x xs)))
(define min
  (lambda (x . xs)
    (fold-left (lambda (a b) (if (> a b) b a)) x xs)))

(define gcdi
  (lambda (a b)
    (if (zero? b)
        a
      (gcdi b (mod a b)))))
(define gcd
  (lambda xs
    (if (null? xs)
        0
      (fold-left (lambda (a b) (gcdi a b)) (car xs) (cdr xs)))))

(define lcmi (lambda (a b) (/ (* a b) (gcdi a b))))
(define lcm
  (lambda xs
    (if (null? xs)
        1
      (fold-left (lambda (a b) (lcmi a b)) (car xs) (cdr xs)))))

; cxxr
(define caar (lambda (xs) (car (car xs))))
(define cadr (lambda (xs) (car (cdr xs))))
(define cdar (lambda (xs) (cdr (car xs))))
(define cddr (lambda (xs) (cdr (cdr xs))))

; cxxxr
(define caaar (lambda (xs) (car (caar xs))))
(define caadr (lambda (xs) (car (cadr xs))))
(define cadar (lambda (xs) (car (cdar xs))))
(define caddr (lambda (xs) (car (cddr xs))))
(define cdaar (lambda (xs) (cdr (caar xs))))
(define cdadr (lambda (xs) (cdr (cadr xs))))
(define cddar (lambda (xs) (cdr (cdar xs))))
(define cdddr (lambda (xs) (cdr (cddr xs))))

(define reverse
  (lambda (xs)
    (fold-left (lambda (a x) (cons x a)) '() xs)))

(define list-tail
  (lambda (xs k)
    (if (zero? k)
        xs
      (list-tail (cdr xs) (- k 1)))))

(define list-ref 
  (lambda (xs k)
    (if (zero? k)
        (car xs)
      (list-ref (cdr xs) (- k 1)))))

(define memq
  (lambda (x ls)
    (if (null? ls)
        #f
        (if (eq? x (car ls))
            ls
          (memq x (cdr ls))))))

(define memv
  (lambda (x ls)
    (if (null? ls)
        #f
        (if (eqv? x (car ls))
            ls
          (memv x (cdr ls))))))

(define member
  (lambda (x ls)
    (if (null? ls)
        #f
        (if (equal? x (car ls))
            ls
          (member x (cdr ls))))))

(define find
  (lambda (p xs)
    (if (null? xs)
        #f
      (if (p (car xs))
          (car xs)
        (find p (cdr xs))))))

(define map-1
  (lambda (f xs)
    (if (null? xs)
        '()
      (cons (f (car xs)) (map f (cdr xs))))))
(define map
  (lambda (f . args)
    (if (memq '() args)
        '()
      (cons (apply f (map-1 car args))
            (apply map f (map-1 cdr args))))))

(define filter
  (lambda (p xs)
    (if (null? xs)
        '()
      (if (p (car xs))
          (cons (car xs) (filter p (cdr xs)))
        (filter p (cdr xs))))))

(define fold-left
  (lambda (f a xs)
    (if (null? xs)
        a
      (fold-left f (f a (car xs)) (cdr xs)))))

(define fold-right
  (lambda (f a xs)
    (if (null? xs)
        a
      (f (car xs) (fold-right f a (cdr xs))))))

(define any
  (lambda (p . xs)
    (if (memq '() xs)
        #f
      (if (apply p (map car xs))
          #t
        (apply any p (map cdr xs))))))

(define every
  (lambda (p . xs)
    (if (memq '() xs)
        #t
      (if (apply p (map car xs))
          (apply every p (map cdr xs))
        #f))))

(define-macro and
  (lambda args
    (if (null? args)
        #t
      (if (null? (cdr args))
          (car args)
        `(if ,(car args) (and ,@(cdr args)) #f)))))

(define-macro or
  (lambda args
    (if (null? args)
        #f
      (if (null? (cdr args))
          (car args)
        `(let ((+value+ ,(car args)))
          (if +value+ +value+ (or ,@(cdr args))))))))

; begin
(define-macro begin
  (lambda args
    (if (null? args)
        `((lambda () '*undef*))
      `((lambda () ,@args)))))

; cond
(define-macro cond
  (lambda args
    (if (null? args)
        '*undef*
      (if (eq? (caar args) 'else)
          `(begin ,@(cdar args))
        (if (null? (cdar args))
            (caar args)
          `(if ,(caar args)
               (begin ,@(cdar args))
            (cond ,@(cdr args))))))))

; case
(define-macro case
  (lambda (key . args)
    (if (null? args)
        '*undef*
      (if (eq? (caar args) 'else)
          `(begin ,@(cdar args))
        `(if (memv ,key ',(caar args))
             (begin ,@(cdar args))
           (case ,key ,@(cdr args)))))))

; do
(define-macro do
  (lambda (var-form test-form . args)
    (let ((vars (map car var-form))
          (vals (map cadr var-form))
          (step (map cddr var-form)))
      `(letrec ((loop (lambda ,vars
                        (if ,(car test-form)
                            (begin ,@(cdr test-form))
                          (begin
                            ,@args
                            (loop ,@(map (lambda (x y)
                                           (if (null? x) y (car x)))
                                           step
                                           vars)))))))
        (loop ,@vals)))))
