; http://www.umcs.maine.edu/~chaitin/unknowable/sets.l
; set.scm : 
;

; Set membership predicate:
(define (member? e set)
  (if (null? set) #f  
  (if (eq? e (car set)) #t  
  (member? e (cdr set)))))

; Subset predicate:
(define (subset? set1 set2)
  (if (null? set1)  #t
  (if (member? (car set1) set2) (subset? (cdr set1) set2)
  #f))) 

; Set union:
(define (union x y)
  (if (null? x) y
  (if (member? (car x) y) (union (cdr x) y)
  (cons (car x) (union (cdr x) y)))))
; Union of a list of sets:
(define (unionl l) 
  (if (null? l) 
    nil 
    (union (car l) (unionl (cdr l)))))

; Set intersection:
(define (intersection x y)
 (if (null? x) nil 
 (if (member? (car x) y) 
   (cons (car x) (intersection (cdr x) y))
   (intersection (cdr x) y))))

; Relative complement of two sets x and y = x - y:
(define (complement x y)
  (if (null? x) nil
  (if (member? (car x) y) 
    (complement (cdr x) y)
    (cons (car x) (complement (cdr x) y)))))

; Cartesian product of an element with a list:
(define (product1 e y) 
  (if (null? y) 
    nil 
    (cons (cons e 
                (cons (car y) 
                      nil))
          (product1 e (cdr y)))))

; Cartesian product of two sets = set of ordered pairs:
(define (product x y)
  (if (null? x) nil 
    (union (product1 (car x) y)  
           (product (cdr x) y))))

; Product of an element with a list of sets:
(define (product2 e y)  
  (if (null? y)
    nil 
    (cons (cons e 
                (car y)) 
          (product2 e (cdr y)))))

; Set of all subsets of a given set:
(define (subsets x)
  (if (null? x)
    '(())
    (let ((y (subsets (cdr x))))
      (union y (product2 (car x) y)))))

