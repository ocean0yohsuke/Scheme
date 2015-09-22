; http://www.serendip.ws/archives/1817

(load "Lib/lib.scm")
(load "Lib/lib2.scm")

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

;;;; apply の定義
(define (apply' procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
          (error
            "Unknown procedure type -- APPLY" procedure))))

;;;; eval の定義: 改良バージョン
(define (eval' exp env)
  ((analyze exp) env))
(define (analyze exp)
  (define (analyze-self-evaluating exp)
    (lambda (env) exp))
  (define (analyze-quoted exp)
    (let ((qval (text-of-quotation exp)))
      (lambda (env) qval)))
  (define (analyze-variable exp)
    (lambda (env) (lookup-variable-value exp env)))

  (define (analyze-assignment exp)
    (let ((var (assignment-variable exp))
          (vproc (analyze (assignment-value exp))))
      (lambda (env)
        (set-variable-value! var (vproc env) env)
        'ok)))
  (define (analyze-definition exp)
    (let ((var (definition-variable exp))
          (vproc (analyze (definition-value exp))))
      (lambda (env)
        (define-variable! var (vproc env) env)
        'ok)))
  (define (analyze-lambda exp)
    (let ((vars (lambda-parameters exp))
          (bproc (analyze-sequence (lambda-body exp))))
      (lambda (env) (make-procedure vars bproc env))))
  (define (analyze-sequence exps)
    (define (sequentially proc1 proc2)
      (lambda (env) (proc1 env) (proc2 env)))
    (define (loop first-proc rest-procs)
      (if (null? rest-procs)
          first-proc
          (loop (sequentially first-proc (car rest-procs))
                (cdr rest-procs))))
    (let ((procs (map analyze exps)))
      (if (null? procs)
          (error "Empty sequence -- ANALYZE"))
      (loop (car procs) (cdr procs))))
  (define (analyze-application exp)
    (let ((fproc (analyze (operator exp)))
          (aprocs (map analyze (operands exp))))
      (lambda (env)
        (execute-application (fproc env)
                             (map (lambda (aproc) (aproc env))
                                  aprocs)))))
  (define (execute-application proc args)
    (cond ((primitive-procedure? proc)
           (apply-primitive-procedure proc args))
          ((compound-procedure? proc)
           ((procedure-body proc)
            (extend-environment (procedure-parameters proc)
                                args
                                (procedure-environment proc))))
          (else
           (error
            "Unknown procedure type -- EXECUTE-APPLICATION"
            proc))))
  (define (analyze-if exp)
    (let ((pproc (analyze (if-predicate exp)))
          (cproc (analyze (if-consequent exp)))
          (aproc (analyze (if-alternative exp))))
      (lambda (env)
        (if (true? (pproc env))
            (cproc env)
            (aproc env)))))
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

;;;; eval の定義: original version
(set! eval (lambda (exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply' (eval (operator exp) env)
                 (list-of-values (operands exp) env)))
        (else
          (error "Unknown expression type -- EVAL" exp)))))

;;;; 手続きの引数
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

;;;; 条件式
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;;;; 並び
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

;;;; 代入と定義
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

;;;; 4.1.2 式の表現

;;;; 自己評価式は数と文字だけ
(define (self-evaluating? exp)
  (cond ((number? exp) #t)
        ((string? exp) #t)
        (else #f)))

;;;; 変数は記号で表現
(define (variable? exp) (symbol? exp))

;;;; クォート式は (quote <text-of-quotation>) の形
(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

;;;; 代入は (set! <var> <value>) の形
(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

;;;; 定義
(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)      ; 仮パラメタ
                   (cddr exp))))    ; 本体

;;;; lambda 式は記号 lambda で始まるリスト
(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;;;; 条件式
(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      #f))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;;;; begin
(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

;;;; 手続き作用
(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

;;;; cond 式
(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      #f
      (let ((first (car clauses))
            (rest (cdr clauses)))
           (if (cond-else-clause? first)
               (if (null? rest)
                   (sequence->exp (cond-actions first))
                   (error "ELSE clause isn't last -- COND->IF"
                          clauses))
               (make-if (cond-predicate first)
                        (sequence->exp (cond-actions first))
                        (expand-clauses rest))))))

;;;; 4.1.3 評価器のデータ構造

;;;; 述語のテスト
(define (true? x)
  (not (eq? x #f)))

(define (false? x)
  (eq? x #f))

;;;; 手続きの表現
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))

;;;; 環境に対する操作
(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))

(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
             (scan (frame-variables frame)
                   (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
             (scan (frame-variables frame)
                   (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
       (define (scan vars vals)
         (cond ((null? vars)
                (add-binding-to-frame! var val frame))
               ((eq? var (car vars))
                (set-car! vals val))
               (else (scan (cdr vars) (cdr vals)))))
       (scan (frame-variables frame)
             (frame-values frame))))

;;;; 4.1.4 評価器をプログラムとして走らせる
(define primitive-procedures
  (list 
    (list 'car car)
    (list 'cdr cdr)
    (list 'cons cons)
    (list 'null? null?)
    (list '> >)
    (list '< <)
    (list '= =)
    (list 'eq? eq?)
    (list '+ +)
    (list '- -)
    (list '* *)
    (list '/ /)
    ;; 基本手続きが続く
    (list 'list list)
    ))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (setup-environment)
  (let ((initial-env
          (extend-environment (primitive-procedure-names)
                              (primitive-procedure-objects)
                              the-empty-environment)))
       (define-variable! 'true #t initial-env)
       (define-variable! 'false #f initial-env)
       initial-env))

(define the-global-environment (setup-environment))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

;(eval '(* 5 5) the-global-environment)
; => 25
;(eval (cons '* (list 5 5)) the-global-environment)
; => 25

;(eval' 
; '((define (loop n) 
;      (if (> n 0) 
;          (loop (- n 1))))
;   (loop 1000))
; the-global-environment)

;;;; 基盤の Lisp システムの"読み込み-評価-印字"ループをモデル化する"駆動ループ(driver loop)"を用意する。
(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
       (let ((output (eval input the-global-environment)))
            (announce-output output-prompt)
            (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(driver-loop)


