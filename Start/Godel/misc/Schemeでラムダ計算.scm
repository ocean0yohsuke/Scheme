; Schemeでラムダ計算 
; http://kreisel.fam.cx/webmaster/clog/img/www.ice.nuie.nagoya-u.ac.jp/~h003149b/lang/lambda.html

(load "Lib/lib.scm")

; Schemeでラムダ計算

; *** car、cdr、cons ***
; car、cdr、consが、lambdaだけで書ける事は、わりと有名。

; (car、cdr、consが書けるというのは、
;  (eq? x (car (cons x y)))
;  (eq? y (cdr (cons x y)))
;  が成立するという事。)

; 次のようになる。

(define _cons
  (lambda (x y)
    (lambda (z)
      (z x y))))
(define _car
  (lambda (z)
    (z (lambda (x y) x))))
(define _cdr
  (lambda (z)
    (z (lambda (x y) y))))

; *** 真偽値、if ***
; 上のを少しまねして、真偽値とifも書ける。

(define _true  (lambda (x y) (x)))
(define _false (lambda (x y) (y)))
(define _if
  (lambda (x y z)
    (x y z) ))

; _if のthen部、else部はlambdaでくくる必要がある。
; 例えば
; 入力:
(_if _true
     (lambda () (+ 10 2))
     (lambda () (* 10 2)))
;
; 結果: 12

; ついでに not も定義しておく。使わないけど。
(define _not
  (lambda (x)
    (_if x
         (lambda () _true)
         (lambda () _false))))

; *** ラムダ式による自然数のコーディング ***
; 自然数を lambda で表現するためにリストを使う。
; だいたいイメージとしては、
; 0 ... (#t . ??)
; 1 ... (#f #t . ??)
; 2 ... (#f #f #t . ??)
; 3 ... (#f #f #f #t . ??)
; のようにする。

; これを _cons、 _true、 _false で実現する。
; ?? の部分は何でもいいけど、ここでは _false にしておく。
; まあ、偽も nil も () も同じといえば同じ。

; とりあえず、定数 0 と、 0 かどうかを判定する関数 _zero? を作る。

(define _zero (_cons _true _true))
(define _zero? 
  (lambda (x)
    (_car x) ))

; 後は、1を足す関数_succ、1を引く関数_predを作る。

(define _succ
  (lambda (x)
    (_cons _false x) ))
(define _pred
  (lambda (x)
    (_cdr x) ))

; 例えば、4は
; (_succ (_succ (_succ (_succ _zero))))
; となる。

; ** 自然数のコード変換 **
; lambda で表した数を実際に表示してもわけが判らないので、
; 普通の数から、lambdaで表した数に変換する関数(とその逆関数)を書いておく。
; ここだけ、lambda,define 以外を使う

(define encode
  (lambda (n)
    (letrec ((loop (lambda (n code)
      (if (zero? n)
          code
          (loop (- n 1) (_succ code))))))
      (loop n _zero))))
(define decode
  (lambda (code)
    (define loop (lambda (code n)
      (if (eq? _zero code)
          n
          (loop (_pred code) (+ n 1)))))
    (loop code 0)))

; 例えば、
; 入力:
(decode (encode 100))
; 結果: 100

; *** いくつかの関数定義 ***
; 基本的な操作が定義できたので、
; 後は再帰的定義の練習問題。

; 加算
(define _plus
  (lambda (x y)
    (_if (_zero? x)
         (lambda () y)
         (lambda () (_plus (_pred x) (_succ y))))))

; 乗算
(define _mult
  (lambda (x y)
    (_if (_zero? x)
         (lambda () _zero)
         (lambda () (_plus y (_mult (_pred x) y))))))

; 階乗
(define _fact
  (lambda (x)
    (_if (_zero? x)
         (lambda () (_succ _zero))
         (lambda () (_mult x (_fact (_pred x)))))))

(decode (_fact (encode 1)))
(decode (_fact (encode 2)))
(decode (_fact (encode 3)))
(decode (_fact (encode 4)))
(decode (_fact (encode 5)))
; => 1
; => 2
; => 6
; => 24
; => 120

; 例
; 入力:
;(decode (_fact (encode 7)))
; 結果: 5040
;
; メモリを無駄に使いすぎるので、大きい数の計算はできない。

; 実際のラムダ計算では define みたいなものは無いので、
; 再帰的定義のためには不動点演算子を使う。
;(define Y
;  (lambda (l)
;    ((lambda (s) (l (lambda (x) ((s s) x))))
;     (lambda (s) (l (lambda (x) ((s s) x)))))))
; 例
; 入力:
((Y
  (lambda (fact)
    (lambda (n)
      (if (= n 0)
          1
          (* n (fact (- n 1)))))))
 10)
; 結果: 3628800
; (面倒なので、普通の数と関数を使った)
;
; このYの定義だと1引数関数の定義にしか使えないので、
; Yの定義を変えたり関数をカリー化したりする。

; lambda だけで算術が扱えるので、
; チューリングマシンで計算できる事はラムダ計算でもできそうなのがわかる。

