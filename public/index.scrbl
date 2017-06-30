#lang scribble/base

; This page can be viewed at
; https://justinpombrio.github.io/RacketSchool/public/index.html

@title{Racket Summer School: Mystery Languages} 

A Mystery Language is a programming language where you know the
@italic{syntax} for the language, but don't know the
@italic{semantics}. To discover it, you must run programs in the
language.

Sometimes a real programming language is a mystery language, and to
really understand a feature of the language you have to experiment,
because the documentation only gives a high-level summary. This same
mindset is also useful when thinking about security:
security vulnerabilities have to be found through clever
experimentation, because they are (almost by definition) the cases
that no one thought of.

These mystery language assignments are an exaggerated form of this:
there is no documentation beyond a syntax definition. Your
job is to discover through experimentation the semantics of a language
feature. Afterwards, you will implement this feature in Redex.

All of these languages will be built up from the same basic language,
with this syntax:

@verbatim{
(define-language basic-syntax
  (p ::= (prog f ... e))
  (f ::= (defun (x x) e))
  (e ::=
   ;; booleans
     true
     false
     (if e e e)
   ;; numbers
     number
     (zero? e)
     (e + e)
   ;; strings
     string
     (empty? e)
     (e ++ e)
   ;; functions & lets
     (e e)
     variable-not-otherwise-mentioned
     (let ((x e)) e)))
}

@section{Installation}

To install the mystery languages, in DrRacket [FILL].

Alternatively, run from the command line:
@verbatim{
raco pkg install https://github.com/justinpombrio/RacketSchool
}

@section{Mystery Language 1: Records}

[FILL]

@verbatim{
(define-extended-language record-syntax basic-lang
  (e ::= ....
     {(s e) ...}
     (e @literal|{@}| e))
}

[FILL]
