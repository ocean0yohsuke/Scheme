# Scheme
My Scheme(R5RS) interpreter written in Haskell

## Interpreter.hs

    ~/Scheme> ghci
    GHCi, version 7.8.3: http://www.haskell.org/ghc/  :? for help
    Loading package ghc-prim ... linking ... done.
    Loading package integer-gmp ... linking ... done.
    Loading package base ... linking ... done.
    Prelude> :l Interpreter.hs
    *Main> run
    Scheme> 
    "hello, world"
    => "hello, world"
    (+ 1 2)
    => 3

    (load "Lib/lib.scm")
    => "load: Lib/lib.scm"
    (map (lambda (x) (* x x)) '(1 2 3 4 5))
    => (1 4 9 16 25)
    (fold-left + 0 '(1 2 3 4 5 6 7 8 9 10))
    => 55
    
    (define infinite (lambda (n) (cons n (infinite (+ n 1)))))
    => inf
    (car (infinite 1))
    => 1
    (car (cdr (infinite 1)))
    => 2
    
    (define quine (lambda (x) `(,x ',x)))
    => quine
    (quine 'quine)
    => (quine 'quine)
    (define godel (lambda (x) `(is-unprovable (value-of (,x ',x)))))
    => godel
    (godel 'godel)
    => (is-unprovable (value-of (godel 'godel)))

    ; Chaitin's `try` function
    (bits 'a)
    => (0 1 1 0 0 0 0 1 0 0 0 0 1 0 1 0)
    (try 0 '(read-exp) '(0 1 1 0 0 0 0 1 0 0 0 0 1 0 1 0))
    => (success a ())
    (try 0 '(read-exp) (bits 'abc))
    => (success abc ())

## FileInterpreter.hs

    Prelude> :l FileInterpreter.hs
    *Main> run
    Files in 'Start'.
    Input number or command; 'list', 'up' or 're-eval' (abbr. ':l', ':u', ':r', respectively).
    - 0. ../
    - 1. [Godel]
    - 2. [Lib]
    - 3. [UnitTest]
    - 4. [eval]
    [Start]: 3
    Scheme Files in 'Start/UnitTest'.
    Input number or command; 'list', 'up' or 're-eval' (abbr. ':l', ':u', ':r', respectively).
    - 0. ../
    - 1. evaluation.scm
    - 2. infinite_list.scm
    - 3. lib.scm
    - 4. macro.scm
    - 5. macro2.scm
    - 6. quasiquote.scm
    - 7. recursion.scm
    - 8. scope.scm
    - 9. set.scm
    [Start/UnitTest]: 1
    [ begin: 1. Start/UnitTest/evaluation.scm ]
    unittest [sequencial] - Cases: 4  Tried: 4  Errors: 0  Failures: 0
    [ end: 1. Start/UnitTest/evaluation.scm ]
    [Start/UnitTest]: 


