(load "Lib/lib.scm")

; Proof that you can't even deduce what the bits of Omega are

; FAS : Formal Axiomatic System, which is just a mechanism for producing an infinite set of theorems.
; U : Univesal Turing Machine.
; FAS* : a binary program for U, FAS*, which is a binary data. We're going to measure its complexity in bits.
; H(x) denotes the size in bits of the smallest program that makes our standard universal Turing machine compute x.
; Omega(N) is the first N bits of the fractional part of the base-two real number Omega.

;   I'll prove that if the formal axiomatic system has program-size
; complexity N, then it can enable you to determine, to prove what is
; the value of at most N + 15328 bits of the halting probability Omega.
;
;      If a FAS has program-size complexity N, then it can enable
;      you to determine at most N + 15328 bits of Omega
;
;   So how do I show this? Well, I have a Berry paradox kind of proof.
; The idea is that if you could prove a lot of bits of Omega, then Omega wouldn't
; be this irreducible
;      H(Omega) > N - 8000.
; That would give you a way to compress Omega into the axioms of the FAS.
; It would give you too concise a way to calculate Omega. If you could prove
; what the bits of Omega are, you'd do it systematically by searching through
; all possible proofs, and that would give you a way to calculate the bits
; of Omega. That's all we're saying, that it would contradict this
;      H(Omega) > N - 8000.

;   It turns out that you can't prove
; that an N-bit string is algorithmically incompressible if N is larger
; than the complexity of your axioms. You can't prove that an N-bit
; string is algorithmically irreducible if it has more bits than the axioms
; you're using for the proof. And similarly it turns out that with N bits
; of axioms you get at most N + 15328 bits of Omega.
;   I write out a 7480-bit program Pi it's about one page of LISP
; code. Why 7480 bits? Because you have to add that to the constant
; in the inequality for H(Omega(N)) to get the constant in our incompleteness
; result. So the difference between these two constants 6840 and 6840 + 7480 is
; the size of the next program in this course, the last program. Divided
; by 8, you get the size of a LISP expression in characters.
;   And what this LISP program Pi does is this. Using try it starts
; running the formal axiomatic system that you're assumed to be given
; that's N bits of code. So we're looking at this:
;       U(Pi FAS* ...)
; The prefix Pi, the program that outputs the theorems of the FAS,
; and some extra stuff that I'll explain later are concatenated and fed to
; U.
;   Pi starts running the formal axiomatic system using larger and
; larger time bounds, capturing the intermediate output, which are the
; theorems. And it looks at the theorems to see how many bits of Omega it
; got. And I allow partial determinations where you get some of the bits
; but you leave holes with unknown bits between some of the bits that
; you do know.
;   And what Pi does is it looks for a small set of axioms that 
; enable you to prove substantially more bits of Omega than there are in those
; axioms—at least 6870 + 7480 bits more. And then it just fills in the holes, the
; missing bits, which costs just one bit per bit, one bit for each missing
; bit. So Pi's final output and the value of
;        U(Pi FAS* missing bits of Omega)
; will be one of the Omega(N)'s. For some N, it'll be the list of the first N bits
; of Omega. By the way, Pi may not need all the bits of FAS*, because it
; only keeps a finite part of the potentially infinite computation for the
; formal axiomatic system.
;   So if you could use N bits of axioms to get essentially more than
; N bits of Omega—more than N + 6870 + 7480 bits, in fact—then you could fill in
; the missing bits at a cost of one bit each, and you get into trouble with
; this inequality
;        H(Omega(N)) > N - 6870.
; That's the point.

; Show that a formal system of complexity N
; can't determine more than 
; N + 6870 + 7480 bits of Omega.
;
; Formal system is a never halting lisp expression
; that outputs lists of the form (1 0 X 0 X X X X 1 0).
; This stands for the fractional part of Omega,
; and means that these 0,1 bits of Omega are known.
; X stands for an unknown bit.

; Here is the self-delimiting Universal Turing Machine!
(define (U p) (cadr (try 'no-time-limit '(eval (read-exp)) p)))

; Here is the prefix Pi, it looks for a small set of axioms that 
; enable you to prove substantially more bits of Omega than there are in those
; axioms—at least 6870 + 7480 bits more.
(define pi
 '(letrec ((number-of-bits-determined (lambda (theorem)
    (if (null? theorem) 0
      (+ (number-of-bits-determined (cdr theorem))
         (if (eq? 'X (car theorem)) 0 1))))))
  (define (supply-missing-bits theorem)
    (if (null? theorem) nil
      (cons (if (eq? 'X (car theorem))
                (read-bit)
                (car theorem))
            (supply-missing-bits (cdr theorem)))))

  (let ((t 1)
        (fas nil)
        (n (+ 6870 7480)))
  (define (examine theorems)
    (if (null? theorems) #f 
    ; (if (< n (number-of-bits-determined (car theorems)))
    (if (< 1 ; Change n to 1 here so will succeed.
           (number-of-bits-determined (car theorems)))
        (car theorems)
        (examine (cdr theorems)))))
  (define (loop)
    (let* ((v (try t '(eval (read-exp)) fas))
           (theorems (caddr v))
           (theorem (examine theorems)))
    (if theorem (supply-missing-bits theorem)
    (if (eq? (car v) 'success) 'failure
    (if (eq? (cadr v) 'out-of-data)
      (begin
        (set! fas (append fas (append fas (cons (read-bit) nil))))
        (set! n (+ 6870 7480 (length fas)))
        (loop))
    (if (eq? (cadr v) 'out-of-time)
      (begin 
        (set! t (+ t 1))
        (loop))
      'unexpected-condition))))))

  (loop))))
; => pi

(newline)

; Size pi.
(length (bits pi))
; => 7480
; Run pi.
(U (append (bits pi) 
   (append 
     ; Toy formal system with only one theorem, a fractional part of Omega, which is deduced from some set of axioms.
     (bits '(printC '(1 X 0)))
     ; Missing bit of omega that is needed, which is deduced from a new axiom.
     '(1))))
; (1 X 0)
; => (1 1 0)


