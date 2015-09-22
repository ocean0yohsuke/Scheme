; Turing's proof of the unsolvability of the halting problem
; http://www.unfindable.net/article/unknowable/turing.html
; https://web.archive.org/web/20110806065908/http://www.cs.auckland.ac.nz/CDMTCS/chaitin/unknowable/ch4.html
(load "Lib/lib.scm")

; ラッセルのパラドックス： 自分の髭を剃らない村中すべての男の髭を剃る床屋

; チューリングの定理： 任意のプログラムが停止するかどうかを判断できるようなアルゴリズムは存在しない

(newline)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(display "Turing's Proof in Scheme") (newline) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Proof that the halting problem is unsolvable by using
; it to construct a Scheme expression that halts iff it doesn't.

(define (loop) (loop))
(define (opposite z)
  (if (halt? z) 
      (loop) 
      "halted"))
(define turing 
  (lambda (x) (opposite `(,x ',x))
))

(display "(turing 'turing) が停止しないと仮定
")
; (turing turing) decides whether it itself has a value, 
; then does the opposite!
; Here we suppose it doesn't have a value, 
; so it turns out that it does:
(define halt? (lambda (x) 
  (display x) (display " will NOT halt." ) (newline)
  #f))
(turing 'turing)
; (turing 'turing) will NOT halt.
; => "halted"

(display "(turing 'turing) が停止すると仮定
")
; And here we suppose it does have a value, 
; so it turns out that it doesn't.
; It loops forever evaluating itself again and again!
(set! halt? (lambda (x)
  (display x) (display " will halt." ) (newline)
  #t))
;(turing 'turing)
; (turing 'turing) will halt.
; ^CInterrupted.


