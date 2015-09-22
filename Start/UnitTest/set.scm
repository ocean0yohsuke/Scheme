(load "Lib/lib.scm")
(unittest "test" (

  ((let ()
   (define test (lambda (a b c)
     (set! a 2)
     (+ a b c)
   ))
   (test 1 2 3))
   ; => 
   7)
  
  ((let ()
   (define d 4)
   (define (test4 a b c)
     (let ((x 1))
       (set! d 10)
       (+ a b c d)
       ))
   (unittest "test - 4" (
     ((test4 1 2 3)
      ; => 
      16)
     (d
      ; => 
      10)
   ))))

  ((let ()
   (define e 5)
   (define (test5 a b c e)
     (let ((x 1))
       (set! e 10)
       (+ a b c e)
       )
   )
   (unittest "test - 5" (
     ((test5 1 2 3 4)
      ; => 
      16)
     (e
      ; => 
      5)
   ))))
))

(unittest "scope" (
  ((let ((ls (list 1 2 3)))
       (set! ls (list 'a 'b 'c))
       ls)
   ; => 
   (a b c))

  ((let ((ls (list 1 2 3)))
     (let ((ms (list 4 5 6)))
       (set! ls (list 'a 'b 'c))) 
     ls)
   ; => 
   (a b c))

  ((let ((ls (list 1 2 3)))
     (let ((ls (list 4 5 6)))
       (set! ls (list 'a 'b 'c))) 
     ls)
   ; => 
   (1 2 3))

  ((let ((ls (list 1 2 3)))
     (let ((ms (list 4 5 6)))
       (let ((ns (list 7 8 9)))
         (let ((os (list 7 8 9)))
           (set! ms (list 'a 'b 'c))
         )
       )
     ms))
   ; => 
   (a b c))

  ((let ()
   (define a 0)
   (define (f)
     (set! a 1))
   a)
   ; => 
   0)

  ((let ()
   (define (test var)
     (let ((x nil))
       (define (set)
         (set! var 1))
       (set)
     var))
   (unittest "scope - test" ( 
     ((test 0) 1)
   ))))
))

(unittest "set-car!" (
  ((let ((ls (list 1 2 3)))
      (set-car! ls 0)
      ls)
   ; => 
   (0 2 3))

  ((let ()
   (define (test2 var)
     (let ((x nil))
       (define (set)
         (set-car! var 0))
       (set)
       var))
   (test2 (list 1 2 3)))
   ; => 
   (0 2 3))
))

(unittest "set-cdr!" (
  ((let ((ls (list 1 2 3)))
      (set-cdr! ls '(20 30))
      ls)
   ; => 
   (1 20 30))

  ((let ()
   (define (test3 var)
     (let ((x nil))
       (define (set)
         (set-cdr! var '(20 30)))
       (set)
       var))
   (test3 (list 1 2 3)))
   ; => 
   (1 20 30))

  ((let ()
   (define p (list 1 2 3))
   (set-car! (cdr p) 'two)
   (unittest "set-cdr! - p" (
     (p 
      ; => 
      (1 two 3))
     ((let ()
      (set-cdr! p '())
      p)
      ; => 
      (1))
   ))))

  ((let ()
   (define z (list 'a 'b 'c))
   (set-cdr! z 'd)
   z)
   ; => 
   (a . d))
))

(unittest "reference" (
  ((let ()
   (define x '(1 2))
   (set-car! x 4)
   x)
   ; =>
   (4 2))

  ; http://www.geocities.co.jp/SiliconValley-Bay/9285/ELISP-JA/elisp_87.html
  ((let ()
   (define x1 '(a b c))
   (define x2 (cons 'z (cdr x1)))
   (unittest "reference - foobaz" (
     ((let ()
      (set-car! (cdr x1) 'foo) ;; 共有部分のCARを置き換える
      ; 両方のリストが変更されている  
      (unittest "reference - foobaz - 1" (
        (x1 
         ; =>
         (a foo c))         
        (x2
         ; => 
         (z foo c))))))
     ((let ()
      (set-car! x1 'baz) ;; 非共有部分のCARを置き換える
      ; 1つのリストだけが変更されている
      (unittest "reference - foobaz - 2" (
        (x1
         ; =>
         (baz foo c)) 
        (x2
         ; => 
         (z foo c))))))
  ))))

  ((let ()
   (define z (list (cons 'a 'b)
                    (cons 'a 'b)))
   (set-car! (car z) 'aaa)
   z)
   ; => 
   ((aaa . b) (a . b)))
  ((let ()
   (define x (cons 'a 'b))
   (define z (list x x))
   (set-car! (car z) 'aaa)
   z)
   ; => 
   ((aaa . b) (aaa . b)))
))

(unittest "env" (
  ((let ()
   (define env (list ()))
   (unittest "env - f" (
     ((let ()
      (define (f x y)
        (set! x y))
      (f env 'a)
      env)
      ; => 
      (()))
     ((let ()
      (define (f x y)
        (set! (car x) y))
      (f env 'a)
      env)
      ; =>
      (a))
     ((let ()
      (define (f x y)
        (set-car! x y))
      (f env 'a)
      env)
      ; => 
      (a))
   ))
  ))

  ((let ()
   (define env (list (list ())))
   (unittest "env - g" (
     ((let ()
      (define (g var)
        (let ((x (car env)))
          (set! x var)))
      (g 'a)
      env)
      ; => 
      ((())))
     ((let ()
      (define (g var)
        (let ((x (car env)))
          (set-car! x var)))
      (g 'a)
      env)
      ; =>
      ((a)))
     ((let ()
      (define (g var)
        (let ((x (car env)))
          (set! (car x) var)))
      (g 'a)
      env)
      ; => 
      ((a)))
   ))
  ))

  ((let ()
   (define env (list (list ())))
   (unittest "env - h" (
     ((let ()
      (define (h var env)
        (let ((x (car env)))
          (set! x var)))
      (h 'a env)
      env)
      ; => 
      ((())))
     ((let ()
      (define (h var env)
        (let ((x (car env)))
          (set-car! x var)))
      (h 'a env)
      env)
      ; =>
      ((a)))
     ((let ()
      (define (h var env)
        (let ((x (car env)))
          (set! (car x) var)))
      (h 'a env)
      env)
      ; => 
      ((a)))
   ))
  ))
))


(unittest "env" (

; set!
((let ()
(let ((env (cons nil nil)))
(define (f a env)
  (set! env a))
(f 'a env)
env))
; => 
(()))

; set-car!, set-cdr!
((let ()
(let ((env (cons nil nil)))
(define (f a d env)
  (set-car! env a)
  (set-cdr! env d))
(f 'a 3 env)
env))
; => 
(a . 3))

; in macro
((let ()
(let ((env (list ())))
(let ((f (lambda (env) (begin (set-car! env 1)))))
(f env)
env)))
; => 
(1))


((let ()
(let ((env (list ())))
(let ((f (lambda (env) (cond (#t (set-car! env 1))))))
(f env)
env)))
; => 
(1))

))

(unittest "reference - set!" (

((let ()
(define a 3)
(define c (cons a a))
(set! a 1)
(unittest "reference - set! - 1" (
(a
; =>
1)
(c 
; => 
(1 . 1)) ;; TODO: gosh では (3 . 3)
((let ()
(set-car! c 4)
; => 
c)
(4 . 1)) ;; TODO: gosh では (4 . 3)
))))

))

