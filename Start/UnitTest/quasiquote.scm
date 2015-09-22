(define a '(1 2 3))
(unittest "quasiquote" (
  ( `(a b c)         (a b c) )
  ( `(,a b c)        ((1 2 3) b c) )
  ( `(,@a b c)       (1 2 3 b c) )
  ( `(,(car a) b c)  (1 b c) )
  ( `(,(cdr a) b c)  ((2 3) b c) )
  ( `(,@(cdr a) b c) (2 3 b c) )
  ( `(z ,(car a))    (z 1) )
  ( `(,@a)           (1 2 3) )
  ( `(z ,@(cdr a))   (z 2 3) )

  ( `(a ,a a)   (a (1 2 3) a) )
  ( `(a 'a ',a) (a 'a '(1 2 3)) )
))


`(a ,@1)
; => 1

;(stacktracelength 0)
;`(a ,@1 b)
; => invalid form: (,@1 b)

