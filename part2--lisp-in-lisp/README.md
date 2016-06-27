# second workshop for prosperity and happiness -- a lisp in lisp

Thank you very much for participating! Here's the code from the workshop + a little more extended language (let, cond, list -- check in the code).

[hola v1](dynamic-lisp.scm) -- our first interpreter, with dynamic scoping,

[hola v2](static-lisp.scm) -- the improved one, with static (lexical) scoping.

[a bonus](Alicante-Lexical-Lisp-interpreter.scm) -- the extended version (including destructuring lambdas, let and cond), plus a few examples for it:

* [vectors, matrices](little-linear-algebra.all) -- definitions of a few non-trivial procedures;

* [something dark](darkness.all) -- the simplified Y combinator for function of 1 and 2 arguments;  cat darkness.all | guile Alicante-Lexical-Lisp-interpreter.scm

[js eval](js-eval-kinda.js) -- an attempt at rewriting ["dynamic"] Eval in js (seems to work).
[elixir eval+parser](elixir-eval-and-parser.exs) -- Eval in elixir, plus a crude SExpr parser.