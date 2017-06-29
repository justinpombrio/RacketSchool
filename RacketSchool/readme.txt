To run the mystery languages, use

    (require "mystery.rkt")

This provides nine languages:

    records1
    records2
    records3
    functions1
    functions2
    functions3
    variables1
    variables2
    variables3

You can run them with:
    (run <language> <program>)

After a language has been run,
    (stuck? <answer>)
will return a boolean saying whether the result of running that
program was a stuck state.

Programs in the languages have the following grammar:
    program P:
      (prog (defun (f x) E) ... E)
    expression E:
      // booleans
        true
        false
        (if E E E)
      // strings
        "a string"
        (empty? E)
        (E ++ E)
      // numbers
        1234
        (zero? E)
        (E + E)
      // let & apply
        (let ((x E)) E)
        (E E)
      // records only:
        {("field" E) ...} (e @ e)
      // variables only:
        (set! x E) (begin E ...)
        programs may also contain (defvar x v)

The mystery languages are defined in:
    mystery-records.rkt
    mystery-functions.rkt
    mystery-variables.rkt"

They build upon two 
"base.rkt"
contains two base languages, arith-lang and base-lang, that the
mystery languages build upon.
