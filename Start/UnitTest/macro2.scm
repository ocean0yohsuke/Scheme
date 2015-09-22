(load "Lib/lib.scm")

(unittest "begin" (
((let ((begin 
  (lambda-macro args 
      (if (null? 'args)
        `((lambda () '*undef*))
        `((lambda () ,@args))))))
(begin 1 2 3 4 5))
; => 
5)
))

(unittest "cond" (
((let ((Y
;(lambda (l)
;  ((lambda (s) (l (lambda (x) ((s s) x))))
;   (lambda (s) (l (lambda (x) ((s s) x))))))
;(lambda (l)
;  ((lambda (s) (l ((s s))))
;   (lambda (s) (l ((s s))))))
(lambda (g)
  ((lambda (s) (g (lambda x (apply (s s) x))))
   (lambda (s) (g (lambda x (apply (s s) x))))))
;))
;(lambda (g)
;  ((lambda (s) (g (apply (s s))))
;   (lambda (s) (g (apply (s s))))))
))
(let ((begin
  (lambda-macro args
    (if (null? args)
        `((lambda () '*undef*))
      `((lambda () ,@args))))))
(let ((cond (Y (lambda (cond)
 (lambda-macro args
    (if (null? args)
        '*undef*
      (if (eq? (caar args) 'else)
          `(begin ,@(cdar args))
        (if (null? (cdar args))
            (caar args)
          `(if ,(caar args)
               (begin ,@(cdar args))
            (cond ,@(cdr args)))))))))))
(let ((cond-test
(lambda (x)
  (cond ((eq? x 'a) 1)
        ((eq? x 'b) 2)
        ((eq? x 'c) 3)
        (else       0)))))
(unittest "cond - result" (
((cond-test 'a) 1)
((cond-test 'b) 2)
((cond-test 'c) 3)
((cond-test 'd) 0)
))
)))))
))

(unittest "case" (
((let ((Y
;(lambda (l)
;  ((lambda (s) (l (lambda (x) ((s s) x))))
;   (lambda (s) (l (lambda (x) ((s s) x))))))
;(lambda (l)
;  ((lambda (s) (l ((s s))))
;   (lambda (s) (l ((s s))))))
(lambda (g)
  ((lambda (s) (g (lambda x (apply (s s) x))))
   (lambda (s) (g (lambda x (apply (s s) x))))))
;(lambda (g)
;  ((lambda (s) (g (apply (s s))))
;   (lambda (s) (g (apply (s s))))))
))

(let ((begin
  (lambda-macro args
    (if (null? args)
        `((lambda () '*undef*))
      `((lambda () ,@args))))))

; case
(let ((case (Y (lambda (case)
  (lambda-macro (key . args)
    (if (null? args)
        '*undef*
      (if (eq? (caar args) 'else)
          `(begin ,@(cdar args))
        `(if (memv ,key ',(caar args))
             (begin ,@(cdar args))
           (case ,key ,@(cdr args))))))))))

(let ((case-test
  (lambda (x)
    (case x
      ((a b c) 1)
      ((d e f) 2)
      ((g h i) 3)
      (else    0)))))
(unittest "case - result" (
((case-test 'a) 1)
((case-test 'e) 2)
((case-test 'i) 3)
((case-test 'j) 0)
))
)))))
))

(unittest "do" (
((let ((Y
;(lambda (l)
;  ((lambda (s) (l (lambda (x) ((s s) x))))
;   (lambda (s) (l (lambda (x) ((s s) x))))))
;(lambda (l)
;  ((lambda (s) (l ((s s))))
;   (lambda (s) (l ((s s))))))
(lambda (g)
  ((lambda (s) (g (lambda x (apply (s s) x))))
   (lambda (s) (g (lambda x (apply (s s) x))))))
;(lambda (g)
; ((lambda (s) (g (apply (s s))))
;   (lambda (s) (g (apply (s s))))))
))
(let ((begin
  (lambda-macro args
    (if (null? args)
        `((lambda () '*undef*))
      `((lambda () ,@args))))))
;do
(let ((do (Y (lambda (do)
  (lambda-macro (var-form test-form . args)
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
        (loop ,@vals))))))))
(let ((fact-do (lambda (x)
       (do ((n 1 (+ n 1))
            (result 1 (* result n)))                   
           ((> n x) result)))))
    
(fact-do 10)
))))
; => 
3628800)


((let ((n 7)) ; do フォーム内の n に影響があるか調べる
(let ((Y
(lambda (g)
  ((lambda (s) (g (lambda x (apply (s s) x))))
   (lambda (s) (g (lambda x (apply (s s) x))))))
))
(let ((begin
  (lambda-macro args
    (if (null? args)
        `((lambda () '*undef*))
      `((lambda () ,@args))))))
;do
(let ((do (Y (lambda (do)
  (lambda-macro (var-form test-form . args)
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
        (loop ,@vals))))))))
(let ((fact-do (lambda (x)
       (do ((n 1 (+ n 1))
            (result 1 (* result n)))                   
           ((> n x) result)))))
    
(fact-do 10)
)))))
; => 
3628800)
))

