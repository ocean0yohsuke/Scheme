(define inf (lambda (n) (cons n (inf (+ n 1)))))

(car (inf 1))

(car (cdr (inf 1)))

(car (cdr (cdr (cdr (inf 1)))))


; => inf
; => 1
; => 2
; => 4

