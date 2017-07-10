(test-begin "evaluator" 27)

;; evaluate one expression, always blank environment
(define (evalg expr) (eval expr (setup-environment)))

;; evaluate multiple expressions, start with blank environment,
;; retain environment
(define (evalg-exprs exprs)
  (let evalg-iter ((env (setup-environment)) (last-result '()) (expr-l exprs))
    (if (null? expr-l)
        last-result
        (evalg-iter env (eval (car expr-l) env) (cdr expr-l)))))

;; test the global environment?

;; primitives
(test-equal "primitive: car"
  'a (evalg '(car '(a b))))
(test-equal "primitive: cdr"
  '(b) (evalg '(cdr '(a b))))
(test-equal "primitive: cons"
  '(a . b) (evalg '(cons 'a 'b)))
(test-equal "primitive: null?, true"
  #t (evalg '(null? '())))
(test-equal "primitive: null?, false"
  #f (evalg '(null? 'a)))


;; special forms
(test-equal "quote symbol"
  'a (evalg '(quote a)))
(test-equal "quote list"
  '(a b c) (evalg '(quote (a b c))))

(test-equal "set!"
  'b (evalg '(let ((x 'a)) (set! x 'b) x)))

(test-equal "begin expression"
  'b (evalg '(begin 'a 'b)))

(test-equal "define variable"
  11 (evalg-exprs '((define x-test 11)
                    x-test)))
(test-equal "define function"
  'b (evalg-exprs '((define (cadr x) (car (cdr x)))
                    (cadr '(a b c)))))

(test-equal "if true expression"
  1 (evalg '(if true 1 2)))
(test-equal "if false expression"
  2 (evalg '(if false 1 2)))

(test-equal "simple lambda"
  5 (evalg '((lambda (x) x) 5)))
(test-equal "compound lambda"
  'b (evalg '((lambda (x) (car (cdr x))) '(a b))))

;; derived expressions
(test-equal "cond standard expression, first true"
  1 (evalg '(cond (true 1) (false 2))))
(test-equal "cond standard else expression"
  2 (evalg '(cond (false 1) (else 2))))
(test-equal "cond procedure"
  '(2) (evalg '(cond ((cdr '(a b 2)) => cdr) (else #f))))

(test-equal "let standard expression, simple"
  1 (evalg '(let ((x 1)) x)))
(test-equal "let standard expression, multiple variables"
  '(1 . 2) (evalg '(let ((x 1) (y 2)) (cons x y))))
(test-equal "let standard expression, multiple lines"
  'a (evalg '(let ((x '(a b c))) (cdr x) (car x))))
(test-equal "named let expression"
  '(a a a)
  (evalg '(let append-recursive ((a '(a)) (count '((()))))
            (if (null? count)
                a
                (append-recursive (cons (car a) a) (car count))))))

(test-equal "and expression, 2nd argument"
  2 (evalg '(and 1 2)))
(test-equal "and expression, false"
  #f (evalg '(and 1 false 2)))

(test-equal "or expression, 1st argument"
  1 (evalg '(or 1 false)))
(test-equal "or expression, last argument"
  3 (evalg '(or false false 3)))
(test-equal "or expression, false"
  #f (evalg '(or false false)))

;; test the loop somehow


(test-end)
