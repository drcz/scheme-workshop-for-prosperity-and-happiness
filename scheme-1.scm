;;; SCHEME FOR PROSPERITY AND HAPPINESS, PART I,
;;; -- version 1.0, 2015-07-31

;;; Hi!
;;; Sorry for bad style, typos and stuff but I wanted to finish this asap and have it done;
;;; at some spare time I will probably improve it (or not). feel free to contact me on suggestions, errors, or questions.

;;; Good.

;;; Symbolic Expressions (SExps but we'll call them SEX for they are sexy) are atoms and cons cells;
;;; Atoms include symbols (e.g. factorial, append, +, if), closures (they represent procedures, we'll get back to these)
;;; and "the empty list" () often called "nil";
;;; normally atoms also include some numerals, strings, booleans, and often other "black-box data types" (e.g. vectors/arrays).

;;; Cons cell is a pair of Symbolic Expressions (e.g. (2 . 3), (factorial . ())).

;;; we use cons cells to represent lists. a list of three numerals 1, 2 and 3  is represented as (1 . (2 . (3 . ()))).
;;; of course lisp parser prefers the concise notation, so we write this list as (1 2 3).

;;; probably you have deduced from the definition of cons cell that it does not have to represent a list, e.g. (2 . 3).
;;; these objects are called dotted pairs and get quite handy for implementing key-value structures.
;;; [also we can a "malformed lists" like (1 . (2 . 3)); these we write as  (1 2 . 3).
;;; they also are useful, but you will not see why during this introduction.
;;; just keep in mind that nothing is wasted.

;;; evey symbolic expression has it's value, which is a symbolic expression.
;;; all atoms but symbols evaluate to themselves [notice in scheme the empty list does not; in a way it's meaningless!].
42
"hi there"
#t

;;; symbols act as variables (i.e. placeholders).
;;; unless you assign them a value, they "don't evaluate" and the interpreter gets pissed off.
;;; we will see assignment in a minute.

;;; a list is being interpreted as a procedure application, e.g.
(+ 2 3)

;;; this is prefix, parenthesized notation ("polish cambridge"), i.e. we take the [value of the] first element of the list
;;; as an operator, and the rest as operands. lisp interpreters evaluate operands first and then apply the operator to them,
;;; so you can easily nest the applications, e.g. 7*8+3 would be
(+ (* 7 8) 3)

;;; a primitive procedures include arithmetic operations +, -, *, /, modulo, (perhaps some more...)
;;; and there are some predicates i.e. procedures evaluating to booleans only:
(eq? 2 3)

(eq? 2 2)

(number? 3)
(number? #t)
(symbol? 3)

;;; we cannot go on further in linear order without two special forms...
;;; when I wrote that lists are interpreted as procedure applications, I lied.
;;; some of them do, but there are so-called special forms, which do not evaluate all of their arguments.
;;; one is the quote form. it does not touch it's argument, just passes it as a value
(quote hola!)

;;; which we abbreviate
'hola!

;;; the second special form is define, which assigns some expression to given symbol
(define x 5)
x
(+ x 3)
;;; (yup, when I told you we'll see assignment in a minute I meant that).

;;; note it does evaluate it's second argument, e.g.
(define x (+ 2 3)
x

;;; now we can get into primitive procedures for cons cells:
;;; car and cdr are the left and right element of the cons cell resp.
(car '(2 . 3))
(cdr '(2 . 3))

(define a-list '(q w e))
(car a-list)
(cdr a-list)
(car (cdr a-list))


;;; the last one we can abbreviate
(cadr a-list)

;;; yes, you can
(caddr a-list)

;;; no time for the last two special forms of this workshop

;;; if is a conditional expression; it evaluates its first argument and if it's #f becomes the value of the third, otherwise the second:
(define x 5)
(if (eq? x 5) 'high-five! 'boo)
(if (eq? x 7) 'high-five! 'boo)

;;; the last one is the sacred one. it's called lambda, the name comes from the Brazilian dance lambada.
(lambda (x) (* x x))

;;; no it does not. yes, it's the anonymous function [in general anonymous procedure].

;;; it's for creating procedures. a procedure is an atom, it's not a symbol so it evaluates to itself.
;;; which means we can use it as any other [primitive] procedure
((lambda (x) (* x x)) 5)

;;; now combined with assignment we can give it some nice name
(define square (lambda (x) (* x x)))

;;; and use it just like we use the primitive operators
(+ (square 2) (square 3))

;;; that's "the ball of dung principle" -- you roll your ball of dung, add some more dung, and still get ball of dung.

;;; notice three facts:
;;; a) the language has very regular syntax, new procedures act just as the primitive ones;
;;; b) you can use any expression in any positions, e.g.
((if (eq? x 3) square (lambda (x) (- x 1))) 5)
;;; c) lisp code is just a bunch of SEXes; this property is called homoiconicity. hey, the machine code has this property too!

;;; (a)-(c) are part of lisp's coolness. 

;;; now you know all of lisp, and most of scheme. go home!



;;; or, let's try to write some code.

;;; the classical example of factorial function (the "n! = 1*2*...*n" thing).
;;; the recursive definition of factorial is 0! = 1, (n+1)! = (n+1)*n!.
;;; which we encode in lisp like this

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(factorial 4)

;;; yes, the only way to do "the looping" is trough recursive calls.
;;; no, I do believe it is more natural for humans to think of, for many reasons "BUT THIS MARGIN IS TOO NARROW TO CONTAIN THEM",
;;; if you excuse the pun.

;;; oh, now notice I didn't write
(define wrong-factorial
  (lambda (n)
    (if (= n 0)
	1
	(* n (wrong-factorial (- n 1))))))
;;; because I was afraid this will not work as wrong-factorial used inside the lambda expression
;;; wasn't defined and define evaluates it's second argument before the assignment.
;;; but of course it does the assignment BEFORE you apply it, so "the meaning" for "wrong-factorial"
;;; is now defined. sorry for that. [anyway if you knew special form let, there it would definitely not work.]

;;; anyway, it's more concise to use define that way, i.e. naming procedure with it's arguments,
;;; and it has more benefits which you will not see for now.

;;; how about concatenating two lists, e.g. (q w e) and (1 2 3) to get (q w e 1 2 3) ?
;;; it's just about replacing the empty list at the end of (q w e) with the list (1 2 3), right?
;;; but, since SEX lists are single-directed structures, we need to traverse the first list and reconstruct it.
;;; well, to be precise, we don't need to, the interpreter will do that.
(define (append xs ys)
  (if (eq? xs '())
      ys
      (cons (car xs)
	    (append (cdr xs) ys))))

(append '(h o) '(l a))

;;; take some time to contemplate how this one works.

;;; now as we can concatenate lists, we can reverse one:

(define (reverse xs)
  (if (eq? xs '())
      '()
      (append (reverse (cdr xs))
	      (cons (car xs) '()))))

(reverse '(dercz likes lisp))

;;; but that's a lousy way to implement reverses, because it traverses [pieces of] xs many times
;;; (since append always has to traverse it's first argument).

;;; we could do much better

(define (rev2 xs acc)
  (if (eq? xs '())
      acc
      (rev2 (cdr xs)
	    (cons (car xs) acc))))

(rev2 '(h o l a) '())
(rev2 '(h o l a) '(a))

;;; and now
(define (reverse xs) (rev2 xs '()))

;;; what it does it traverses xs [only once!] and conses its consecutive elements onto acc [for accumulating] list,
;;; which it returs as it's result. note also that this is a tail-recursive function, i.e. the last thing it does is
;;; the call (to itself). therefore it does not need to put any trash on the call stack.

;;; however I advise you to forget about the code efficiency for now.

;;; yes you can write efficient programs in lisp, as well as I can write inefficient code in c/Java/php etc.

;;; now two assignments. I will not write the answers, as you did very well:

;;; 1) define "length" procedure, which takes a list and returns it's length,
;;; e.g.. (length '(q w e)) => 3, (length '()) => 0.

;;; 2) define "sum" and "product" procedures, from list of numbers into a single number being their sum/product resp.
;;; e.g. (sum '(1 2 3 4)) => 10, (prod '(1 2 3 4)) => 24.

;;; you did that?
;;; having problem?
;;; keep in mind you always have to use prefix notation, so (+ 2 3), not (2 + 3).
;;; also remember this:
;;; MATH    LISP
;;; f(x) ~> (f x)

;;; so, does it work? cool, well done!

;;; you may notice how similar the definitions of append, length, sum and product are.
;;; we can abstract that pattern. it's called fold-right and is a monoid homomorphism,
;;; so we'll call it reduce and not mention monoids:
(define (reduce xs id op)
  (if (eq? xs '())
      id
      (op (car xs)
	  (reduce (cdr xs) id op))))

;;; now we can...
(reduce '(1 2 3 4) 0 +)
(reduce '(1 2 3 4) 1 *)

;;; the homomorphism stuff: reduce replaces empty list with id, and cons with op, so
(cons 1 (cons 2 (cons 3 (cons 4 '()))))
;;; converts into
(+ 1 (+ 2 (+ 3 (+ 4 0))))

;;; so e.g.
(define (prod xs) (reduce xs 1 *))
(define (sum xs) (reduce xs 0 +))
(define (length xs) (reduce xs 0 (lambda (h t) (+ 1 t))))
;;; test them!

;;; this one I find brilliant:
(define (append xs ys) (reduce xs ys cons))
(append '(q w e) '(1 2 3))

;;; etc.


;;; as Antonio suggested, we could compute factorial with this easily:

;;; compose list of n positive natural numbers:
(define (num n)
  (if (= n 0)
      '()
      (cons n (num (- n 1)))))
(num 5)

;;; so now:
(define (fact2 n) (prod (num n)))
(fact2 4)
;;; [at the end of this file I will get back to computing factorial in a crazy way.]



;;; now time for more higher order functions, and closures:
(define (mk-adder n) (lambda (x) (+ x n)))
(mk-adder 3)
((mk-adder 3) 2)
((mk-adder 5) 2)

;;; note that the values of mk-adder is a procedure of 1 arugment which "remembers" the value of n as it was during it's creation.
;;; that's the power of lexical scoping. please do contemplate this example.

;;; now a little more magic and we'll be done for now:

(define (compose f g) ;; (A->A) x (A->A) -> (A->A)
  (lambda (x) (f (g x))))

(define (succ n) (+ n 1))
(define (square n) (* n n))

;;; which one is 10, and which one is 16?
((compose succ square) 3)
((compose square succ) 3)

;;; now suppose we would like to compose some procedure with itself, n times,

(define (n-fold-f n f)
  (if (= n 0)
      (lambda (x) x)
      (lambda (x) (f ((n-fold-f (- n 1) f) x)))))

;;; oh yeah, we do have "=" [for numerals], I just used eq? to be more oldschool.

((n-fold-f 3 succ) 5)
((n-fold-f 3 square) 2)

;;; and the last magical operation, currying (in honour of Haskell Curry):

(define (n-fold n)
  (lambda (f)
    (if (= n 0)
	(lambda (x) x)
	(lambda (x) (f (((n-fold (- n 1)) f) x))))))

(((n-fold 3) succ) 5)

;;; if you do understand this construction, you are THAT close to enlightement.

;;; now we stopped here.
;;; the conclusions are the following:
;;; with a bunch of primitive functions and special forms we do have an extremely elegant, complete programming language.
;;; of course the crappy procedures we devised are all in standard libraries, namely in SRFI-1.
;;; I only wanted you to see/implement them in order to get the feeling.




;;; now some bonuses:

(define (map f xs)
  (if (null? xs)
      '()
      (cons (f (car xs)) (map f (cdr xs)))))

;;; ah yes... (null? _) is just equivalent of (eq? _ '()).
;;; another assignment: rewrite it using reduce.

;;; you know this guy from other languages.
(map square '(1 2 3 4))

;;; we can implement the [binary] Cartesian product with that
(define (cart xs ys)
  (map (lambda (y)
	 (map (lambda (x)
		(cons x y))
	      xs))
       ys))

(cart '(q w e) '(1 2))


;;; now the tricky part: the hardest recursive equation ever.
;;; assignment: define procedure perm which takes a list and returns all the permutations of it's elements, e.g.
;; (perm '(q w e)) => ((q w e) (q e w) (w q e) (w e q) (e w q) (e q w))
;;; give it a try if you dare.


;;; I will show you how to implement it, but even after that try to re-create this result.

;;; 1. the idea is to induce on the length of the list.
;;; there is exactly one permutation of the empty list -- itself, so the result on boundary case if easy: (()).
(define (perm xs)
  (if (null? xs)
      '(())
      'no-idea))
;;; (note it's the 1-element list containing an empty list, since we want to return list of permutations, i.e. list of lists.

;;; 2. and now things get complicated...
;;; but we can take each element of the list and then cons it at the end of each permutation of all the remaining ones.
;;; in order to have these we need a "drop" procedure which removes element from the list:
(define (drop x xs)
  (if (eq? x (car xs))
      (cdr xs)
      (drop x (cdr xs))))

(drop 'w '(q w e))

;;; yeah, that's ugly one, it works only in the case when there's exactly one occurrence of x in xs; it's ok for our task, but
;;; in general we would rather prefer something like:
(define (drop x xs)
  (cond ((null? xs) '())
	((eq? x (car xs)) (drop x (cdr xs)))
	(#t (cons (car xs) (drop x (cdr xs))))))

(drop 'w '(q w e w q w))
(drop 'x '(q w e))

;;; ah yes. cond is a special form which generalizes if. try for yourself...

;;; nice. assignment: define drop using reduce.

;;; ok so let's try our "permutation equation"
(define (perm xs)
  (if (null? xs)
      '(())
      (map (lambda (x)
	     (map (lambda (p)
		    (cons x p))
		  (perm (drop x xs))))
	   xs)))

(perm '(1))
(perm '(1 2))
(perm '(1 2 3))

;;; well, that's kinda close, but something gets wrong with the nesting.

;;; any ideas?




;;; yes, that's because "the outer map" gets a separate list of permutations;
;;; so we need to concatenate these, in order to have a single list of permutations.
;;; there are several ways to do this...
;;; but to keep the spirit of minimalism let's use the old good reduce:
(reduce '((q w e) (1 2 3) (hi there)) '() append)

;;; got it?

(define (perm xs)
  (if (null? xs)
      '(())
      (reduce (map (lambda (x)
		     (map (lambda (p)
			    (cons x p))
			  (perm (drop x xs))))
		   xs)
	      '()
	      append)))

(perm '(q w e))

;;; yeah!

;;; and now, the last thing, the most crazy [and inefficient] way to compute the factorial.
;;; as you may have notices, the number of permutations of n-element list (set in general) is n!.
;;; therefore we could write:
(define (factorial n)
  (length (perm (num n))))

(factorial 5)

;;; that's sick. good night.




























;;; hmmm, still in here?
;;; ok so let's play a little with reverse polish notation; in case of arithmetics it's something like this:
;;; the expression (2+3)*7 we can write as
(define rpn1 '(2 3 + 7 *))
;;; provided the operations have fixed arity this is a completely disambiguous form "2 3 + 7 *".
;;; some might recognize this notation e.g. from HP calculators.
;;; in order to compute the value of such an expression we read it from left to right, memorizing numbers
;;; and applying each operator to two [assuming all operators are binary] last memorized numbers.
;;; that can be mimicked with a stack, e.g.

;;; current expr "2 3 + 7 *", stack: () [empty]
;;; current expr "3 + 7 *",   stack: (2)
;;; current expr "+ 7 *",     stack: (3 2)
;;; current expr "7 *",       stack: (5)
;;; current expr "*",         stack: (7 5)
;;; current expr "",          stack: (35)

;;; the result is the only remaining numeral on the stack, which seems correct as (2+3)*7 = 35.
;;; we can easily code this in scheme. we will use new special form let in order to make it more readable.
;;; let creates local assignments, visible only inside the expression, e.g.
(let ((a 2)
      (b (+ 1 2)))
  (+ a b))

;;; so, let's do this!

(define (calculator expr stack)
  (if (null? expr)
      (car stack)
      (let ((current (car expr))
	    (expr (cdr expr)))
	(cond ((number? current)
	       (calculator expr (cons current stack)))
	      ((eq? '+ current)
	       (calculator expr (cons (+ (car stack)
					 (cadr stack))
				      (cddr stack))))
	      ((eq? '* current)
	       (calculator expr (cons (* (car stack)
					 (cadr stack))
				      (cddr stack))))
	      ((eq? '- current)
	       (calculator expr (cons (- (cadr stack)
					 (car stack))
				      (cddr stack))))
	      ((eq? '/ current)
	       (calculator expr (cons (/ (cadr stack)
					 (car stack))
				      (cddr stack))))))))

(calculator rpn1 '())

;;; note that for non-commutative actions (- and /) we need to keep the order, as the stack "reverses everything"
;;; (cf our second implementation of reverse).

;;; now in principle we can use the stack as the list of input data...
;;; for example to compute ((a * b) + (3 * 7) for arbitrary a and b we could place a and b on the stack; e.g. a=2, b=3:
(calculator '(* 3 7 * +) '(3 2))

;;; 2*3+3*7=9*3=27, correct!

;;; but we cannot compute any expression like this, e.g. (a*a)+(b*b) is a problem.
;;; however we can borrow to commands form classical stack-based languages: DUP and SWAP:
;;; DUP duplicates the top of the stack, eg (2 3 1) -> (2 2 3 1),
;;; SWAP swaps first two elements of the stack, eg (2 3 1) -> (3 2 1).

;;; that's easy!

(define (calculator expr stack)
  (if (null? expr)
      (car stack)
      (let ((current (car expr))
	    (expr (cdr expr)))
	(cond ((number? current)
	       (calculator expr (cons current stack)))
	      ((eq? '+ current)
	       (calculator expr (cons (+ (car stack)
					 (cadr stack))
				      (cddr stack))))
	      ((eq? '* current)
	       (calculator expr (cons (* (car stack)
					 (cadr stack))
				      (cddr stack))))
	      ((eq? '- current)
	       (calculator expr (cons (- (cadr stack)
					 (car stack))
				      (cddr stack))))
	      ((eq? '/ current)
	       (calculator expr (cons (/ (cadr stack)
					 (car stack))
				      (cddr stack))))
	      ((eq? 'DUP current)
	       (calculator expr (cons (car stack) stack)))
	      ((eq? 'SWAP current)
	       (calculator expr (cons (cadr stack)
				      (cons (car stack)
					    (cddr stack)))))
	      ;;; any more commands?
	      ))))

;;; so we can expess the sum-of-squares program like this:
(calculator '(DUP * SWAP DUP * +) '(2 3))

;;; to make it easier to undestand here's the sequence of (expr x stack) states:
expr: (DUP * SWAP DUP * +) stack: (2 3)
expr: (* SWAP DUP * +)     stack: (2 2 3)
expr: (SWAP DUP * +)       stack: (4 3)
expr: (DUP * +)            stack: (3 4)
expr: (* +)                stack: (3 3 4)
expr: (+)                  stack: (9 4)
expr: ()                   stack: (13)

;; nb: if you want to have similar "traces" just add the following line:
  (write `(expr: ,expr stack: ,stack)) (newline)
;; between "(define (calculator..." and "(if (null?...".

;;; now in order to build a [forth-like] language with this thing we need to introduce at least some user-defined named expressions...
;;; but that's really the task for another time.
;;; also it would be interesting to compile expressions like (+ (* a a) (* b b)) into this "calculator code",
;;; but at the moment I have no idea how to do this and should probably go to bed.
;;; in part II perhaps we could do this, as a nice pretext to learn some more scheme (and Olin Shiver's incredible matcher).

;;; anyway thanks for your time, good night and happy hacking!

;;; -- the end --