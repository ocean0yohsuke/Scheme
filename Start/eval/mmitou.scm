;　http://d.hatena.ne.jp/mmitou/20100819

(load "Lib/lib.scm")
(load "Lib/lib2.scm")

(define true #t)
(define false #f)
(set! error (lambda (arg . args)
  (if (null? args) 
      (begin 
        (display arg) (newline)
        (exit))
      (begin 
        (display arg) (display " ")
        (map (lambda (x) 
               (begin (display x) (display " ")))
             args)
        (newline)
        (exit)))))
;
; mylisp.scm
;

;; BEGIN frame functions
(define (first-frame env)
  (car env))
(define (extend-environment vars vals env)
  (define (make-frame variables values)
    (map cons variables values))
  (cons (make-frame vars vals) env))
(define (lookup-var-val-in-frame var frame)
  (assoc var frame))
(define (lookup-var-val var env)
  (if (null? env)
      (error "not found:" var)
      (let ((var-val (lookup-var-val-in-frame var (first-frame env))))
        (if var-val
            var-val
            (lookup-var-val var (cdr env))))))
(define (add-binding-to-frame var val frame)
  (cons (cons var val) frame))
(define (define-var-val! var val env)
  (let ((frame (first-frame env)))
    (let ((var-val (assoc var frame)))
      (if var-val
      (set-cdr! var-val val)
      (set-car! env (add-binding-to-frame var val frame))))))
;; END frame functions

;; BEGIN eval functions
(set! eval (lambda (exp env)
  (define (syntax var)
    (cadr var))
  (define (operator var)
    (car var))
  (define (operands exp)
    (cdr exp))
  (define (application? var)
    (or (eq? (car var) 'procedure)
        (eq? (car var) 'primitive)))
  (define (self-evaluating? exp)
    (cond 
     ((number? exp) #t)
     ((string? exp) #t)
     (else #f)))
  (define (variable? proc)
    (symbol? proc))
  (define (list-of-values exps env)
    (map (lambda (x) (eval x env)) exps))
  (define (syntax? var)
    (eq? (car var) 'syntax))
  (cond
   ((self-evaluating? exp) exp)
   ((variable? exp) (cdr (lookup-var-val exp env)))
   ((list? (operator exp))
      (apply'
        (eval (operator exp) env)
        (list-of-values (operands exp) env)))
   (else
     (let ((var (cdr (lookup-var-val (car exp) env))))
      (cond
       ((syntax? var) ((syntax var) exp env))
       ((application? var)
          (apply' 
            (eval (operator exp) env)
            (list-of-values (operands exp) env)))
       (else
         (error "Unknown exp -- EVAL" exp))))))))

(define (eval-sequence exps env)
  (define (last-exp? exps)
    (null? (cdr exps)))
  (define (first-exp exps)
    (car exps))
  (define (rest-exps exps)
    (cdr exps))
  (cond
   ((last-exp? exps) (eval (first-exp exps) env))
   (else
    (eval (first-exp exps) env)
    (eval-sequence (rest-exps exps) env))))
;; END eval functions

;; BEGIN apply functions
(define (apply' proc args)
  (define (tagged-list? proc symbol)
    (if (pair? proc)
        (eq? (car proc) symbol)
        #f))
  (define (apply-primitive-proc proc args)
    (define (primitive-implementation proc)
      (cadr proc))
    (apply (primitive-implementation proc) args))
  (define (primitive-proc? proc)
    (tagged-list? proc 'primitive))
  (define (compound-proc? proc)
    (tagged-list? proc 'procedure))
  (define (proc-body proc)
    (caddr proc))
  (define (proc-params proc)
    (cadr proc))
  (define (proc-env proc)
    (cadddr proc))
  (cond
   ((primitive-proc? proc)
    (apply-primitive-proc proc args))
   ((compound-proc? proc)
    (eval-sequence
     (proc-body proc)
     (extend-environment
      (proc-params proc)
      args
      (proc-env proc))))
   (else
    (error "Unknown proc -- APPLY" proc))))
;; END apply functions

;; BEGIN eval-if
(define (eval-if exp env)
  (define (if-predicate exp)
    (cadr exp))
  (define (if-consequent exp)
    (caddr exp))
  (define (if-alternative exp)
    (if (not (null? (cdddr exp)))
        (cadddr exp)
        'false))
  (if (eval (if-predicate exp) env)
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))
;; END eval-if

;; BEGIN eval-quote
(define (eval-quote exp env)
  (define (text-of-quotation exp)
    (cadr exp))
  (text-of-quotation exp))
;; END eval-quote

;; BEGIN eval-set!
(define (eval-set! exp env)
  (define (assignment-variable exp) (cadr exp))
  (define (assignment-value exp) (caddr exp))
  (define (set-var-val! var val env)
    (let ((var-val (lookup-var-val-in-frame var 
                   (first-frame env))))
      (if var-val
      (set-cdr! var-val val)
      (error "not found:" var))))
  (set-var-val! 
   (assignment-variable exp)
   (eval (assignment-value exp) env)
   env)
  'ok)
;; END  eval-set!

;; BEGIN eval-define
(define (eval-define exp env)
  (define (define-variable exp)
    (if (symbol? (cadr exp))
        (cadr exp)
        (caadr exp)))
  (define (define-value exp)
    (if (symbol? (cadr exp))
        (caddr exp)
        (make-lambda (cdadr exp)
             (cddr exp))))
  (define (make-lambda vars body)
    (cons 'lambda (cons vars body)))
  (define-var-val! 
    (define-variable exp)
    (eval (define-value exp) env)
    env)
  'ok)
;; END eval-define

;; BEGIN eval-lambda
(define (eval-lambda exp env)
  (define (lambda-params exp) (cadr exp))
  (define (lambda-body exp) (cddr exp))
  (define (make-procedure parameters body env)
    (list 'procedure parameters body env))
  (make-procedure 
   (lambda-params exp)
   (lambda-body exp)
   env))
;; END eval-lambda

(define primitive-procs
  (list 
   (list 'car 'primitive car)
   (list 'cdr 'primitive cdr)
   (list 'cons 'primitive cons)
   (list 'null? 'primitive null?)
   (list 'if 'syntax eval-if)
   (list 'quote 'syntax eval-quote)
   (list 'set! 'syntax eval-set!)
   (list 'define 'syntax eval-define)
   (list 'lambda 'syntax eval-lambda)
   (list 'begin 'syntax eval-sequence)
   (list '> 'primitive >)
   (list '< 'primitive <)
   (list '= 'primitive =)
   (list 'eq? 'primitive eq?)
   (list '+ 'primitive +)
   (list '- 'primitive -)
   (list '* 'primitive *)
   (list '/ 'primitive /)

   (list 'list 'primitive list)
   (list 'newline 'primitive newline)
   (list 'display 'primitive display)
    
   ))
(define (setup-environment)
  (define (primitive-procedure-names procs)
    (map car procs))
  (define (primitive-procedure-values procs)
    (map cdr procs))
  (let ((init-env 
     (extend-environment
      (primitive-procedure-names primitive-procs)
      (primitive-procedure-values primitive-procs)
      '())))
  (extend-environment
   (list 'true 'false) 
   (list #t #f) 
   init-env)))
(define the-global-environment (setup-environment))

; (eval '(* 5 5) the-global-environment)
; => 25
; (eval (cons '* (list 5 5)) the-global-environment)
; => 25

;; BEGIN driver-loop
(define (driver-loop)
  (newline)
  (display "> ")
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (display output)))
  (driver-loop))
;; END driver-loop
(driver-loop)

