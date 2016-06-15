(use-modules (srfi srfi-1)
	     (ice-9 match)
	     (ice-9 pretty-print))

(define (error msg) (pretty-print msg) #f) ;; best error recovery ever... not.

(define (lookup sym env)
  (let ((val (assoc sym env)))
    (if val
	(cdr val)
	(error `(unbound symbol ,sym)))))

(define (update sym val env) `((,sym . ,val) . ,(alist-delete sym env )))

(define (bool->T/nil b) (if b 'T '()))

(define (T/nil? expr) (or (eq? expr 'T) (null? expr)))
(define (primop? sym) (member sym '(car cdr cons + - * / % = < atom? num? input print pprint newline apply)))
(define (self-evaluating? expr)
  (or (T/nil? expr)
      (number? expr)
      (primop? expr)
      (and (pair? expr)
	   (eq? (car expr) '&proc))))


;;; sugar!
(define (cond->ifs cond-cases)
  (fold-right (lambda (h t) `(if ,(car h) ,(cadr h) ,t))
	      '()
	      cond-cases))

(define (let->lambda bindings body)
  (let* ([names (map car bindings)]
	 [vals (map cadr bindings)])
    `((^ ,names ,body) . ,vals)))

;;; destructuring!
(define (bind args vals)
  (match args
    [(? symbol? arg) `([,arg . ,vals])]
    [((? symbol? arg1) . (? symbol? arg2))
     `([,arg1 . ,(car vals)] [,arg2 . ,(cdr vals)])]
    [_ (append-map bind args vals)]))

;;; the thing!
(define (Eval expr env topenv)
  (let E [(expr expr)]
    (match expr

      [(? self-evaluating? e) e]
      
      [(? symbol? s) (lookup s (append env topenv))]
      
      [('quote e) e]

      [('if condition then else) (if (eq? 'T (E condition)) (E then) (E else))]
      [('^ args body) `(&proc ,args ,body ,env)]

      [('cond . cond-cases) (E (cond->ifs cond-cases))]
      [('let bindings body) (E (let->lambda bindings body))]
      
      [(rator . rands)
       (match (map E expr)
	 [('car e) (car e)]
	 [('cdr e) (cdr e)]
	 [('cons e1 e2) (cons e1 e2)]
	 [('+ e1 e2) (+ e1 e2)]
	 [('- e1 e2) (- e1 e2)]
	 [('* e1 e2) (* e1 e2)]
	 [('/ e1 e2) (* 1.0 (/ e1 e2))]
	 [('% e1 e2) (modulo e1 e2)]
	 [('= e1 e2) (bool->T/nil (equal? e1 e2))]
	 [('< e1 e2) (bool->T/nil (and (number? e1) (number? e2) (< e1 e2)))]
	 [('atom? e) (bool->T/nil (not (pair? e)))]
	 [('num? e) (bool->T/nil (number? e))]
	 
	 [('input) (begin (write 'input>) (read))]
	 [('print e) (begin (display e) e)]
	 [('pprint e) (begin (pretty-print e) e)]
	 [('newline) (begin (newline) 'T)]

	 [('list . es) es]
	 
	 [(('&proc args body closure) . vals)
	  (Eval body (append (bind args vals) closure) topenv)]
	 
	 [_ (error `(application error ,rator to ,rands))])])))


(define (repl output topenv)
  (begin
    (pretty-print output)
    (write '>)
    (match (read)
      [('quit) (begin (pretty-print 'Hasta-pronto!) (quit))]
      [('*topenv*) (repl topenv topenv)]
      [('def name expr)
       (let ((val (Eval expr '() topenv)))
	 (if val
	     (repl `(new binding for ,name) (update name val topenv))
	     (repl 'sorry topenv)))]
      [expr
       (let ((val (Eval expr '() topenv)))
	 (if val
	     (repl val topenv)
	     (repl 'sorry topenv)))])))

(begin
  (pretty-print `(---- Alicate Lexical Lisp 1.0 ----))
  (pretty-print `((c) 2016 Alicate Tech Meetup group))
  (newline)
  (repl 'READY. '()))
