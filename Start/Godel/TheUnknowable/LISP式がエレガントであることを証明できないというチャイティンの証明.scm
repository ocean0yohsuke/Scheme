; My proof that you can't show that a LISP expression is elegant
; http://www.unfindable.net/article/unknowable/chaitin.html
; https://web.archive.org/web/20131212225205/http://www.cs.auckland.ac.nz/CDMTCS/chaitin/unknowable/ch5.html

; ベリーのパラドックス（ラッセル版）： 20文字以下で記述できない最初の自然数 （←19字）

; チャイティンの定理： 公理の計算量+N よりも大きい計算量を持つ Lisp式 のエレガント性は証明できない
;                : エレガント性は計算できない

(load "Lib/lib.scm")
(load "Lib/lib2.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(display "My Proof that You Can't Show that a LISP Expression is Elegant") (newline)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; すべての可能な証明が番号の付けられたリストになっているとする。
; N 番目の証明が正しければそれが証明する定理を返し、そうでなければ nil を返すような形式公理系 (fas N) も実装されているとする。

; 証明がエレガント性の証明 (is-elegant x) であり且つそのサイズが処理系のサイズ (size b) よりも大きければ
; 定理 x を返すような関数 b を作ることができる
; これはベリーのパラドックス（エレガント性の定義と矛盾）である。
; 矛盾を避けるためには、「FASが不健全」か「FASの計算量+N よりも大きい計算量を持つ Lisp式 のエレガント性は、FASでは証明できない」である必要がある。

(display "#1 Here it doesn't find an elegant expression larger than it is:") (newline)
; Berry expression
; This expression searches for an elegant expression
; that is larger than it is and returns the value of
; that expression as its own value.
(define b (lambda ()
  ; formal axiomatic system
  (define (fas n)
    (cond ((= n 1) '(is-elegant x))
          ((= n 2) nil)
          ((= n 3) '(is-elegant yyy))
          (#t 'stop)))
  (define (repeat n)
    (let ((theorem (print (fas n))))
      (if (equal? nil theorem) (repeat (+ n 1))
        (if (equal? theorem 'stop) 'fas-has-stopped
          (if (equal? (car theorem) 'is-elegant)
             (if (> (print (size (cadr theorem))) (print (size b)))
                 (eval (cadr theorem))
                 (repeat (+ n 1)))
             (repeat (+ n 1)))))))
  (repeat 1)))
(b)
; (is-elegant x)
; 1
; 491
; ()
; (is-elegant yyy)
; 3
; 491
; stop
; => fas-has-stopped

(newline)
(display "#2 Here it finds an elegant expression exactly one character larger than it is:") (newline)
(set! b (lambda ()
  ; formal axiomatic system
  (define fas (lambda (n)
    (cond ((= n 1) '(is-elegant x))
          ((= n 2) nil)
          ((= n 3) '(is-elegant yyy))
          ((= n 4) (cons 'is-elegant (cons (^ 10 568) nil))) ; ★  an elegant expression exactly one character larger than b 
          (#t 'stop))))
  (define (repeat n)
    (let ((theorem (print (fas n))))
      (if (equal? nil theorem) (repeat (+ n 1))
        (if (equal? theorem 'stop) 'fas-has-stopped
          (if (equal? (car theorem) 'is-elegant)
             (if (> (print (size (cadr theorem))) (print (size b)))
                 (eval (cadr theorem))
                 (repeat (+ n 1)))
             (repeat (+ n 1)))))))
  (repeat 1)))
(b)
; (is-elegant x)
; 1
; 568
; ()
; (is-elegant yyy)
; 3
; 3
; (is-elegant "1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000")
; 569
; 568
; => "1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"

(newline)
(display "#3 Here it finds an elegant expression exactly the same size as it is:") (newline)
(set! b (lambda ()
  ; formal axiomatic system
  (define fas (lambda (n)
    (cond ((= n 1) '(is-elegant x))
          ((= n 2) nil)
          ((= n 3) '(is-elegant yyy))
          ((= n 4) (cons 'is-elegant (cons (^ 10 567) nil))) ; ★
          (#t 'stop))))
  (define (repeat n)
    (let ((theorem (print (fas n))))
      (if (equal? nil theorem) (repeat (+ n 1))
        (if (equal? theorem 'stop) 'fas-has-stopped
          (if (equal? (car theorem) 'is-elegant)
             (if (> (print (size (cadr theorem))) (print (size b)))
                 (eval (cadr theorem))
                 (repeat (+ n 1)))
             (repeat (+ n 1)))))))
  (repeat 1)))
(b)
; (is-elegant x)
; 1
; 568
; ()
; (is-elegant yyy)
; 3
; 568
; (is-elegant "100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000")
; 568
; 568
; stop
; => fas-has-stopped

(newline)
(display "#4 Here it finds an elegant expression much larger than it is, and evaluates it:") (newline)
(set! b (lambda ()
  ; formal axiomatic system
  (define fas (lambda (n)
    (cond ((= n 1) '(is-elegant x))
          ((= n 2) nil)
          ((= n 3) '(is-elegant yyy))
          ((= n 4) (cons 'is-elegant (cons (cons '- (cons (^ 10 700) (cons 1 nil))) nil))) ; ★
          (#t 'stop))))
  (define (repeat n)
    (let ((theorem (print (fas n))))
      (if (equal? nil theorem) (repeat (+ n 1))
        (if (equal? theorem 'stop) 'fas-has-stopped
          (if (equal? (car theorem) 'is-elegant)
             (if (> (print (size (cadr theorem))) (print (size b)))
                 (eval (cadr theorem))
                 (repeat (+ n 1)))
             (repeat (+ n 1)))))))
  (repeat 1)))
(b)
; (is-elegant x)
; 1
; 598
; ()
; (is-elegant yyy)
; 3
; 598
; (is-elegant (- 1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000 1))
; 707
; 598
; => 999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999



