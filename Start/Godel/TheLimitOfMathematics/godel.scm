(load "Lib/lib.scm")

; ベリーのパラドックス（ラッセル版）: 20文字以下で記述できない最初の自然数 （←19字）

; チャイティンの定理: 公理の計算量+N よりも大きい計算量を持つ Lisp式 のエレガント性は証明できない
;                : エレガント性は計算できない

; Show that a formal system of lisp complexity
; H_lisp(FAS) = N cannot enable us to exhibit
; an elegant S-expression of size greater than N + 370.
; An elegant lisp expression is one with the property
; that no smaller S-expression has the same value.
;
; Postulation for reductio ad absurdum: 
; Formal Axiomatic System is a never-ending
; lisp expression that displays 'elegant' S-expressions.

; Idea is to have a program P search for something X that can be proved
; to be more complex than P is, and therefore P can never find X.
; I.e., idea is to show that if this program halts we get a contradiction,
; and therefore the program doesn't halt.

(size '(printC (^ 10 352)))
; => 19

; Here is the key expression.
;
(define berry
         
 '(let* {
    ; Here we are given the formal axiomatic system FAS.
    [fas '(printC (^ 10 352))]
    ; N = the number of characters in this program including the FAS.
    [n (+ 333 (size fas))]}  ; 352 = 333 + 19

  ; Examine list x for element that is more than n characters in size.
  ; If not found returns false.
  (define (examine theorems)
    (if (null? theorems) #f
    (if (< n (size (car theorems)))
      (car theorems)
      (examine (cdr theorems)))))

  ; Loop running the formal axiomatic system.
  (define (loop t)
    (let* ((v (try t fas nil))                 ; Run the formal system for T time steps.
           (theorems (caddr v)) 
           (s (examine theorems)))             ; Did it output an elegant s-exp larger than this program?
      (if s (eval s)                           ; If found elegant s-exp bigger than this program,
                                               ; run it so that its output is our output. Contradiction!
      (if (eq? 'failure (car v)) (loop (+ t 1) ; If not, keep looping,
      'failure)))))                            ; or halt if formal system halted.

  (loop 0)                                     ; Start loop running with T = 0.
))
; => berry

(newline)

; Run expression & show that it knows its own size
; and can find something bigger than it is.
(eval berry)
;    10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
; => 10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000

(size (^ 10 352))
; => 353
(size berry)
; => 352

;   This contradicts the definition of elegance, because this
;      (352 characters (formal axiomatic system))
; large LISP expression is at least one character too small to produce
; that value.
;   So either the formal axiomatic system was lying, and produced a false theorem
; , or in fact this
;      (352 characters (formal axiomatic system))
; won't work, because it will never find the elegant LISP expression that
; it's searching for, it will never find an elegant LISP expression larger
; than it is, it will never find an elegant LISP expression that's more than
; 352 characters bigger than the formal axiomatic system that it's using.



