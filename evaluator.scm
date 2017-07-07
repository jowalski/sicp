(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)
;; self-evaluating items
(define (self-evaluating? exp)
  (cond ((number? exp) #t)
        ((string? exp) #t)
        (else #f)))

;; variables
(define (variable? exp) (symbol? exp))

;; quotations
(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))


(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

;; assignments
(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

;; definitions
(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
                   (cddr exp)))) ; body

;; lambda expressions
(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;; conditionals
(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;; begin expressions
(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

;; procedure applications
(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))
;; as special forms (add these to special forms list)
(define (eval-and exp env)
  (if (null? exp)
      #t
      (let ((value (eval (car exp) env)))
        (if (false? value)
            #f
            (if (null? (cdr exp))
                value
                (eval-and (cdr exp) env))))))

(define (eval-or exp env)
  (if (null? exp)
      #f
      (let ((value (eval (car exp) env)))
        (if (true? value)
            value
            (eval-or (cdr exp) env)))))
;; ;; as derived expressions
;; (define (eval-and exp env) (eval (and->if exp) env))
;; (define (eval-or exp env) (eval (or->if exp) env))
;; 
;; ;; an example of and turned into a derived expression of ifs
;; ;; (and a b c) =>
;; ;; (if (null? exp)
;; ;;     'true
;; ;;     (if (a)
;; ;;         (if (b)
;; ;;             (if (c)
;; ;;                 c
;; ;;                 'false)
;; ;;             'false)
;; ;;         'false))
;; ;; (or a b c) =>
;; ;; (if (null? exp)
;; ;;     'false
;; ;;     (if (a)
;; ;;         a
;; ;;         (if (b)
;; ;;             b
;; ;;             (if (c)
;; ;;                 c
;; ;;                 'false))))
;; 
;; (define (and->if exp)
;;   (expand-and-clauses (cdr exp)))
;; 
;; (define (expand-and-clauses clauses)
;;   (if (null? clauses)
;;       'true
;;       (let ((first (car clauses))
;;             (rest (cdr clauses)))
;;         (make-if first
;;                  (if (null? rest)
;;                      first
;;                      (expand-and-clauses rest))))))
;; 
;; (define (or->if exp)
;;   (expand-or-clauses (cdr exp)))
;; 
;; (define (expand-or-clauses clauses)
;;   (if (null? clauses)
;;       'false
;;       (let ((first (car clauses))
;;             (rest (cdr clauses)))
;;         (make-if first (expand-or-clauses rest)))))
(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

;; (cond a b c) =>
;; (if (null? exp)
;;     'false
;;     ;; ((lambda (x)
;;     ;;    (if (x)
;;     ;;        (if (eq? (car actions) '=>)
;;     ;;            ((cdr actions) x)
;;     ;;            x)
;;     ;;        (lambda (y)
;;     ;;          (if (y)
;;     ;;              (if (eq? (car)))))))
;;     ;;  (cond-predicate a))
;;     (if (cond-predicate a)
;;         (sequence->exp (cond-actions a))
;;         (if (cond-predicate b)
;;             (sequence->exp (cond-actions b))
;;             (if (cond-predicate c)
;;                 (sequence->exp (cond-actions b))
;;                 'false))))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no `else' clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (let ((pred (cond-predicate first))
                  (actions (cond-actions first)))
              (make-cond-clause pred actions rest))))))

(define (make-cond-type actions pred-var)
  (if (eq? (car actions) '=>)
      (list (cdr actions) 'x)
      (sequence->exp actions)))

(define (make-cond-clause pred actions rest)
  (list (make-lambda 'x
                     (make-if 'x
                              (make-cond-type actions 'x)
                              (expand-clauses rest)))
        pred))
(define (let-vars var-exp)
  (if (null? var-exp)
      '()
      (cons (car var-exp) (let-vars (cdr var-exp)))))

(define (let-exps var-exp)
  (if (null? var-exp)
      '()
      (cons (cadr var-exp) (let-exps (cdr var-exp)))))

(define (let-var-exp exp) (cadr exp))

(define (let-body exp) (cddr exp))

(define (let->combination exp)
  (list (make-lambda (let-vars (let-var-exp exp))
                     (let-body exp))
        (let-exps (let-var-exp exp))))
(define (make-let var-exps body)
  (list 'let var-exps body))

(define (let*->nested-lets exp)
  (define (expand-let-clauses var-exps)
    (if (null? var-exps)
        (let-body exp)
        (make-let (list (car var-exps))
                  (expand-let-clauses (cdr var-exps)))))
  (expand-let-clauses (let-var-exp exp)))
(define (derived-exp exp-f)
  (lambda (exp env) (eval (exp-f exp) env)))

(define special-forms
  (list
   `(quote  . ,(lambda (exp env) (text-of-quotation exp)))
   `(set!   . ,eval-assignment)
   `(define . ,eval-definition)
   `(if     . ,eval-if)
   `(lambda . ,(lambda (exp env)
                 (make-procedure (lambda-parameters exp)
                                 (lambda-body exp) env)))
   `(begin  . ,(lambda (exp env) (eval-sequence (begin-actions exp) env)))
   `(cond   . ,(derived-exp cond->if))
   `(and    . ,eval-and)
   `(or     . ,eval-or)
   `(let    . ,(derived-exp let->combination))))
(define (enclose-body body) (list (make-lambda '() body)))
(define (make-value-definition var value)
  (list 'define var value))
(define (make-fun-definition var args body)
  (list 'define (list var args) body))
(define (enclose-body body) (list (make-lambda '() body)))
(define (make-value-definition var value)
  (list 'define var value))
(define (make-fun-definition var args body)
  (list 'define (list var args) body))
(define (named-let-var exp) (cadr exp))
(define (named-let-vars exp) (let-vars (caddr exp)))
(define (named-let-exps exp) (let-exps (caddr exp)))
(define (named-let-body exp) (cadddr exp))

(define (make-named-let var vars exps body)
  (enclose-body
   (list (make-value-definition var
           (make-lambda vars body))
         (cons var exps))))

(define (let->combination exp)
  (if (symbol? (cadr exp))
      (make-named-let (named-let-var exp)     ; named let
                      (named-let-vars exp)
                      (named-let-exps exp)
                      (named-let-body exp))
      (list (make-lambda (let-vars (let-var-exp exp)) ; regular let
                         (let-body exp))
            (let-exps (let-var-exp exp)))))
(define (true? x)
  (not (eq? x #f)))

(define (false? x)
  (eq? x #f))
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))
(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))

(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))
(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    initial-env))

;; (define the-global-environment (setup-environment))
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        ;; <MORE PRIMITIVES>
        ))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define apply-in-underlying-scheme apply)
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))
(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((pair? exp)
         (let ((operator (car exp)))
           (let ((special-form (assq-ref special-forms operator)))
             (if special-form
                 (special-form exp env)
                 (apply (eval operator env)
                        (list-of-values (cdr exp) env))))))
        (else (error "Unknown expression type -- EVAL"))))
(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))
 (define the-global-environment (setup-environment))
