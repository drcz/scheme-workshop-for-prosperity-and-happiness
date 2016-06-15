(use-modules (srfi srfi-1) (ice-9 match) (ice-9 pretty-print))

(define (error msg) (pretty-print msg) #f)

(define (lookup key env)
  (let ([val (assoc key env)])
    (if val
	(cdr val)
	(error `(unbound symbol ,key)))))

(define (update key val env) `([,key . ,val] . ,(alist-delete key env)))

(define (bool->T/nil b) (if b 'T '()))

(define (primop? sym)
  (member sym '(car cdr cons + - * / = < atom? num? print input)))

(define (Eval expr env)
  (let E ([expr expr])
    (match expr
      ['T 'T]
      [() '()]
      [(? number? n) n]      
      [('&proc args body) expr]
      [(? primop? p) p]
      [(? symbol? s) (lookup s env )]
      [('quote e) e]
      [('if premise conclusion alternative)
       (if (eq? 'T (E premise)) (E conclusion) (E alternative))]
      [('^ args body) `(&proc ,args ,body)]
      [(rator . rand)
       (match (map E expr)
	 [('+ e1 e2) (+ e1 e2)]
	 [('* e1 e2) (* e1 e2)]
	 [('- e1 e2) (- e1 e2)]
	 [('/ e1 e2) (* 1.0 (/ e1 e2))]
	 [('car e) (car e)]
	 [('cdr e) (cdr e)]
	 [('cons e1 e2) (cons e1 e2)]
	 [('= e1 e2) (bool->T/nil (equal? e1 e2))]
	 [('num? e) (bool->T/nil (number? e))]
	 [('atom? e) (bool->T/nil (not (pair? e)))]
	 [('print e) (begin (pretty-print e) e)]
	 [('input) (begin (write '>) (read))]
	 [(('&proc args body) . vals)
	  (Eval body (append (map cons args vals) env))])])))


(define (repl output topenv)
  (begin
    (pretty-print output)
    (write 'REPL>)
    (match (read)
      [('quit) (begin (pretty-print 'Hasta-pronto!) (quit))]
      [('def name expr)
       (let ([val (Eval expr topenv)])
	 (if val
	     (repl `(new binding for ,name)
		   (update name val topenv))
	     (repl 'sorry! topenv)))]
      [expr
       (let ([val (Eval expr topenv)])
	 (if val
	     (repl val topenv)
	     (repl 'sorry! topenv)))])))

(repl 'READY. '())

      


 





