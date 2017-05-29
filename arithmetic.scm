(define (=zero? x) (apply-generic '=zero? x))
(define (neg x) (apply-generic 'neg x))
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  #f))
            #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))
(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))
(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))
(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum -- CONTENTS"))))
(define (put-coercion source-type target-type proc)
  (put 'coercion (list source-type target-type) proc))

(define (get-coercion source-type target-type)
  (get 'coercion (list source-type target-type)))
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))

  (define (reduce-integers n d)
    (let ((g (gcd n d)))
      (list (/ n g) (/ d g))))

  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (tag (= x y))))
  (put '=zero? '(scheme-number)
       (lambda (x) (= 0 x)))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'raise '(scheme-number)
       (lambda (x)
         (let ((rat_val
                (rationalize (inexact->exact x) 1/1000)))
           (make-rational (numerator rat_val)
                          (denominator rat_val)))))
  (put 'neg '(scheme-number)
       (lambda (x) (tag (- x))))
  (put 'greatest-cd '(scheme-number scheme-number)
       (lambda (x y) (tag (gcd x y))))
  (put 'reduce-arith '(scheme-number scheme-number)
       (lambda (n d) (tag (reduce-integers n d))))

  (put 'make-1 'scheme-number drop)

  (put 'project '(scheme-number) round)
  (put-coercion 'scheme-number 'complex
                (lambda (n) (make-complex-from-real-imag (contents n) 0)))
  (put-coercion 'scheme-number 'rational
                (get 'raise '(scheme-number)))
  'done)
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))

  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational)
       (lambda (x y) (and (= (numer x) (numer y))
                          (= (denom x) (denom x)))))
  (put '=zero? '(rational)
       (lambda (x) (and (=zero? (numer x))
                        (=zero? (denom x)))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))

  (put 'make-1 'rational
       (lambda (x) (let ((rat_val
                          (rationalize (inexact->exact x) 1/1000000)))
                     (make-rational (numerator rat_val)
                                    (denominator rat_val)))))

  (put 'raise '(rational)
       (lambda (x) (make-complex-from-real-imag
                    (/ (numer x)
                       (denom x)) 0)))
  (put 'project '(rational) (lambda (x) (make-scheme-number (/ (numer x) (denom x)))))

  ;; (define (rational-number->complex n)
  ;;    (make-complex-from-real-imag (/ (numer n) (denom n)) 0))
  (put-coercion 'rational 'complex
                (lambda (n) (make-complex-from-real-imag
                             (/ (numer (contents n))
                                (denom (contents n))) 0)))
  'done)
(define (make-rational n d)
  ((get 'make 'rational) n d))
(define (raise x) (apply-generic 'raise x))
(define tag-order '(scheme-number rational complex))
(define (tag-greater t1 t2 tag-order)
  (let ((first (car tag-order)))
    (cond ((eq? t1 first) #f)
          ((eq? t2 first) #t)
          ((null? tag-order) (error "Tags not found in tag-order"))
          (else (tag-greater t1 t2 (cdr tag-order))))))

(define (tag-max ttags)
  (if (null? (cdr ttags))
      (car ttags)
      (let ((max-tag (tag-max (cdr ttags))))
        (if (tag-greater (car ttags) max-tag tag-order)
            (car ttags)
            max-tag))))

(define (raise-to-level x tag)
  (cond ((eq? (type-tag x) tag) x)
        ((eq? tag 'dense) (make-dense-terms `((0 ,x))))
        ((eq? tag 'polynomial) (make-poly '(x) (make-dense-terms `((0 ,x)))))
        (else (raise-to-level (raise x) tag))))

(define (raise-all-to-same-level args)
  (let ((max-tag (tag-max (map type-tag args))))
    (map (lambda (x) (raise-to-level x max-tag)) args)))
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (same? l)
  (if (null? (cdr l))
      #t
      (and (eq? (car l) (cadr l)) (same? (cdr l)))))

(define (all? l)
  (if (null? l)
      #t
      (and (car l) (all? (cdr l)))))

(define (any? l)
  (if (null? l)
      #f
      (or (car l) (any? (cdr l)))))
;; we should not call drop with these generic procedures
;; since they would trigger an infinite loop
;; all generics called in this function, or called in a function
;; called by this function should go in this list
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          ((if (memq op '(raise project equ? =zero? div
                          greatest-cd reduce-arith)) identity drop)
           (apply proc (map contents args)))
          (if (not (same? type-tags))
              (let ((newargs (raise-all-to-same-level args)))
                (apply apply-generic (cons op newargs)))
              (error "No method for these types"
                     (list op type-tags)))))))
(define (project x) (apply-generic 'project x))
(define (drop x)
  (let ((projected-x (project x)))
    (if (equ? x (raise projected-x))
        (if (eq? (type-tag x) (type-tag projected-x))
            projected-x
            (drop projected-x))
        x)))
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2))
                         (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
                         (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                       (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                       (sub (angle z1) (angle z2))))

  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) (lambda (x) (real-part x)))
  (put 'imag-part '(complex) (lambda (x) (imag-part x)))
  (put 'magnitude '(complex) (lambda (x) (magnitude x)))
  (put 'angle '(complex) (lambda (x) (angle x)))
  (put 'neg '(complex) (lambda (x) (neg x)))
  (put 'equ? '(complex complex)
       (lambda (x y) (or (and (equ? (magnitude x) (magnitude y))
                              (equ? (angle x) (angle y)))
                         (and (equ? (real-part x) (real-part y))
                              (equ? (imag-part x) (imag-part y))))))
  (put '=zero? '(complex)
       (lambda (x) (and (=zero? (real-part x))
                        (=zero? (imag-part x)))))
  (put 'project '(complex)
       (lambda (x) ((get 'make-1 'rational) (real-part x))))
  ;; ???
  ;; (put 'raise '(complex) (lambda (x) (real-part x)))
  (put 'raise '(complex) identity)
  'done)

(define (square x) (* x x))
(define (dropf f)
  (lambda (. l)
    (make-1 (apply f (map drop l))
      (type-tag (car l)))))

(define (make-1 x type)
  ((get 'make-1 type) x))

(define sine (dropf sin))
(define atang (dropf atan))
(define cosine (dropf cos))
(define squared (dropf square))
(define sqroot (dropf sqrt))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqroot (add (squared (real-part z))
               (squared (imag-part z)))))
  (define (angle z)
    (atang (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (mul r (cosine a)) (mul r (sine a))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'neg '(rectangular)
       (lambda (x) (make-from-real-imag (neg (real-part x))
                                        (neg (imag-part x)))))
  (put 'raise '(rectangular) identity)
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (mul (magnitude z) (cosine (angle z))))
  (define (imag-part z)
    (mul (magnitude z) (sine (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqroot (add (squared x) (squared y)))
          (atang y x)))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'neg '(polar)
       (lambda (x) (make-from-mag-ang (magnitude x)
                                      (add 3.14159268 (angle x)))))
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'raise '(polar) identity)
  'done)
(install-scheme-number-package)
(install-rectangular-package)
(install-polar-package)
(install-rational-package)
(install-complex-package)
(define (ftag tg) (lambda (f)
                    (lambda (. l)
                      (attach-tag tg (apply f l)))))

(define (put-list type tagfun l)
  (for-each (lambda (x) (put (car x) type (tagfun (cadr x)))) l))
(define (variable? x) (symbol? x))
(define (variables? x)
  (or (variable? x) (all? (map variable? x))))

(define (same-variable? v1 v2)
  (and (variables? v1) (variables? v2) (equal? v1 v2)))
(define (s< a b)
  (string< (symbol->string a) (symbol->string b)))
(define (s> a b)
  (string> (symbol->string a) (symbol->string b)))

(define (sorder a b)
  (if (s< a b) (list a b) (list b a)))

(use-modules (srfi srfi-1))

(define (insert-in-ordered-list sym-l l)
  (define (insert-first sym il)
    (reverse (fold (lambda (x y)
                     (if (s< (car y) x)
                         (cons x y)
                         (cons (car y) (cons x (cdr y)))))
                   (list sym) il)))
  (if (null? sym-l)
      l
      (insert-in-ordered-list (cdr sym-l) (insert-first (car sym-l) l))))

(define (which-s< a b)
  (if (s< a b) a b))
(define (which-s>= a b)
  (if (s< a b) b a))

(define (install-polynomial-package)
  (define (make-poly variable term-list)
    (list variable term-list))

  (define (variable p) (car p))
  (define (term-list p) (cadr p))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add (term-list p1)
                        (term-list p2)))
        (make-poly (insert-in-ordered-list (variable p2) (variable p1))
                   (add-poly-poly (term-list p1)
                                  (term-list p2)))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul (term-list p1)
                        (term-list p2)))
        (make-poly (insert-in-ordered-list (variable p2) (variable p1))
                   (mul-poly-poly (term-list p1)
                                  (term-list p2)))))

  (define (sub-poly p1 p2) (add-poly p1 (neg-poly p2)))
  (define (neg-poly p)
    (make-poly (variable p) (neg (term-list p))))

  (define (equ-poly? p1 p2)
    (and (equal? (variable p1) (variable p2))
         (equ? (term-list p1) (term-list p2))))

  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (div (term-list p1)
                        (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))

  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (greatest-common-divisor (term-list p1) (term-list p2)))
        (error "Polynomials of different variables")))

  (define (reduce-poly n d)
    (if (same-variable? (variable n) (variable d))
        (map (ftag-poly (lambda (x) (make-poly (variable n) x)))
             (reduce-arith (term-list n) (term-list d)))
        (map (ftag-poly identity) `(,n ,d))))

  (define (=zero-poly? x) (=zero? (term-list x)))

  (define ftag-poly (ftag 'polynomial))

  (put-list '(polynomial) ftag-poly
            `((project ,identity)
              (raise ,identity)
              (neg ,neg-poly)))
  (put-list '(polynomial polynomial) ftag-poly
            `((add ,add-poly)
              (mul ,mul-poly)
              (div ,div-poly)
              (sub ,sub-poly)
              (greatest-cd ,gcd-poly)))
  (put-list '(polynomial) identity
            `((=zero? ,=zero-poly?)))
  (put-list '(polynomial polynomial) identity
            `((equ? ,equ-poly?)
              (reduce-arith ,reduce-poly)))
  (put-list 'polynomial ftag-poly
            `((make ,make-poly)))
  'done)

(define (add-poly-poly t1 t2)
  (apply-generic 'add-poly-poly t1 t2))

(define (mul-poly-poly t1 t2)
  (apply-generic 'mul-poly-poly t1 t2))
;; the term-list format is list of (<order> <coeff>) pairs
;; ((3 2) (2 3) (0 1))
(define (install-dense-package)
  ;; internal procedures
  ;; representation of terms and term lists
  (define (make-terms term-list . l)
    (list term-list (if (null? l) '() (car l))))

  (define (terms l) (car l))
  (define (remainder-terms l) (cadr l))

  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))

  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))

  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define ftag-dense (ftag 'dense))

  (define (equ-terms-inner? t1 t2)
    (cond ((and (null? t1) (null? t2)) #t)
          ((or (null? t1) (null? t2)) #f)
          (else (let ((ft1 (first-term t1))
                      (ft2 (first-term t2)))
                  (and (eq? (order ft1) (order ft2))
                       (equ? (coeff ft1) (coeff ft2))
                       (equ-terms-inner? (rest-terms t1) (rest-terms t2)))))))

  (define (apply-to-terms f)
    (lambda (a b) (f (terms a) (terms b))))
  (define (apply-mk-to-terms f)
    (lambda (a b) (apply make-terms `(,(f (terms a) (terms b))))))

  (define equ-terms? (apply-to-terms equ-terms-inner?))

  ;; (define (equ-terms? L1 L2)
  ;;   (let ((t1 (terms L1))
  ;;         (t2 (terms L2)))
  ;;     (equ-terms-inner? t1 t2)))

  (define (add-terms-inner t1 t2)
    (cond ((empty-termlist? t1) t2)
            ((empty-termlist? t2) t1)
            (else
             (let ((f1 (first-term t1))
                   (f2 (first-term t2)))
               (cond ((> (order f1) (order f2))
                      (adjoin-term f1
                                   (add-terms-inner (rest-terms t1) t2)))
                     ((< (order f1) (order f2))
                      (adjoin-term f2
                                   (add-terms-inner t1 (rest-terms t2))))
                     (else
                      (adjoin-term
                       (make-term (order f1)
                                  (add (coeff f1) (coeff f2)))
                       (add-terms-inner (rest-terms t1) (rest-terms t2)))))))))
  (define add-terms (apply-mk-to-terms add-terms-inner))
  ;; (define (add-terms L1 L2)
  ;;   (let ((t1 (terms L1))
  ;;         (t2 (terms L2)))
  ;;     (make-terms (add-terms-inner t1 t2))))

  ;; (use-modules ((srfi srfi-1)))
  ;; add/multiply two polynomials
  (define (add-poly-f addfun tagfun)
    (define (rfun t v)
      (let ((t1 (terms t)))
        (make-terms
         (cond ((null? t1) (list (make-term 0 (tagfun v))))
               ((zero? (order (first-term t1)))
                (list (make-term 0 (tagfun (addfun v (coeff (first-term t1)))))))
               (else (cons (first-term t1)
                           (terms (rfun (make-terms (cdr t1)) v))))))))
    rfun)

  (define add-poly-val (add-poly-f add identity))
  (define add-poly-poly-term (add-poly-f add-poly-val (ftag-dense identity)))

  (define (mul-poly-f f)
    (lambda (t v)
      (let ((t1 (terms t)))
        (make-terms (map (lambda (term)
                           (make-term (order term)
                                      (f v (coeff term)))) t1)))))

  (define mul-poly-val (mul-poly-f mul))
  (define mul-poly-poly-term (mul-poly-f (ftag-dense mul-poly-val)))

  (define (mul-terms-inner t1 t2)
    (if (empty-termlist? t1)
          (the-empty-termlist)
          (add-terms-inner (mul-term-by-all-terms (first-term t1) t2)
                           (mul-terms-inner (rest-terms t1) t2))))
  ;; (define mul-terms (apply-mk-to-terms mul-terms-inner))
  (define (mul-terms L1 L2)
    (let ((t1 (terms L1))
          (t2 (terms L2)))
      (make-terms (mul-terms-inner t1 t2))))

  (define (div-terms-inner L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2))))
                (let ((rest-of-result
                       (div-terms-inner
                        (add-terms-inner
                         L1
                         (mul-terms-inner `((,new-o ,new-c))
                                          (neg-terms-inner L2)))
                        L2)))
                  (list
                   ;; RESULT
                   (add-terms-inner `((,new-o ,new-c))
                                    (car rest-of-result))
                   ;; REMAINDER
                   (cadr rest-of-result))))))))
  
  (define (div-terms TA TB)
    (let ((L1 (terms TA))
          (L2 (terms TB)))
      (div-terms-inner L1 L2)))

  (define (mul-terms-num trms n)
    (map (lambda (trm) (make-term (order trm) (* n (coeff trm)))) trms))
  
  (define (integerizing-factor t1 t2)
    (expt (coeff (first-term t2))
          (+ 1 (- (order (first-term t1)) (order (first-term t2))))))
  
  (define (pseudoremainder-terms-inner t1 t2)
    (let ((int-factor (integerizing-factor t1 t2)))
      (div-terms-inner (mul-terms-num t1 int-factor) t2)))
  
  (define (coeffs-gcd trms)
    (reduce greatest-common-divisor 1 (map coeff trms)))
  
  (define (div-terms-n trms n)
    (map (lambda (trm)
          (make-term (order trm) (div (coeff trm) n))) trms))
  (define (simplify-coeffs trms)
    (div-terms-n trms (coeffs-gcd trms)))
  
  (define (gcd-terms-inner a b)
    (if (empty-termlist? b)
        a
        (simplify-coeffs (gcd-terms-inner b (remainder-terms
                                             (pseudoremainder-terms-inner a b)))))
    )
  
  (define (gcd-terms a b)
    (let ((ai (terms a))
          (bi (terms b)))
      (make-terms (gcd-terms-inner ai bi))))
  

  (define (reduce-terms-inner n d)
    (let ((gcd-n-d (gcd-terms-inner n d)))
      (let ((int-factor (integerizing-factor n d)))
        (let ((p-list (map (lambda (p)
                            (terms (div-terms-inner
                                    (mul-terms-num p int-factor)
                                    gcd-n-d)))
                      (list n d))))
          (let ((gcd-coef (apply greatest-common-divisor
                                 (map coeffs-gcd p-list))))
            (map (ftag-dense
                  (lambda (p) (make-terms (div-terms-n p gcd-coef))))
                 p-list))))))
  
  (define reduce-terms (apply-to-terms reduce-terms-inner))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (neg-terms-inner ts)
    (map (lambda (x) (make-term (order x)
                                (neg (coeff x))))
                     ts))

  (define (neg-terms L)
    (make-terms (neg-terms-inner (terms L))))

  (define (=zero-termlist? x)
    (and (empty-termlist? (terms x))
         (null? (remainder-terms x))))

  (put-list '(dense) ftag-dense
            `((project ,identity)
              (raise ,identity)
              (neg ,neg-terms)))
  (put-list '(dense dense) ftag-dense
            `((add ,add-terms)
              (add-poly-poly ,add-poly-poly-term)
              (mul ,mul-terms)
              (div ,div-terms)
              (mul-poly-poly ,mul-poly-poly-term)
              (greatest-cd ,gcd-terms)))
  (put-list '(dense) identity
            `((=zero? ,=zero-termlist?)))
  (put-list '(dense dense) identity
            `((equ? ,equ-terms?)
              (reduce-arith ,reduce-terms)))
  (put-list 'dense ftag-dense
            `((make-terms ,make-terms)))
  'done)

(define (install-sparse-package)

  (define (the-empty-termlist) '(0 ()))
  (define (first-term term-list)
    (list (highest-order term-list)
          (car (sparse-list term-list))))
  (define (rest-terms term-list)
    (list (- (highest-order term-list) 1)
          (cdr (sparse-list term-list))))
  (define (empty-termlist? term-list)
    (and (null? (sparse-list term-list))
         (<= (highest-order term-list) 0)))

  (define (highest-order term-list)
    (car term-list))
  (define (sparse-list term-list)
    (cadr term-list))

  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (equ-terms? t1 t2)
    (or (and (empty-termlist? t1) (empty-termlist? t2))
        (and (eq? (highest-order t1) (highest-order t2))
             (equ-list? (sparse-list t1) (sparse-list t2)))))

  (define (equ-list? l1 l2)
    (or (and (null? l1) (null? l2))
        (and (equ? (car l1) (car l2))
             (equ-list? (cdr l1) (cdr l2)))))

  ;; for two lists x, y where (length x) >= (length y),
  ;; append the the first ((length x) - (length y)) elements to the list,
  ;; followed by the sum of each subsequent element in the list
  (define (append-add x n y)
    (cond ((and (null? x) (null? y)) #nil)
          ((= n 0) (cons (add (car x) (car y)) (append-add (cdr x) 0 (cdr y))))
          ((> n 0) (cons (car x) (append-add (cdr x) (- n 1) y)))
          (else (error "problem with APPEND-ADD"))))

  ;; return the list of the element x added to the element at the nth position of l
  (define (add-x-to-nth l x n)
    (cond ((= n 0) (cons (add (car l) x) (cdr l)))
          (else (cons (car l) (add-x-to-nth (cdr l) x (- n 1))))))

  (define (sep-and-mul-every-n l x n)
    (if (null? l)
        #nil
        (cons (mul (car l) x)
              (append (make-list n 0)
                      (sep-and-mul-every-n (cdr l) x n)))))

  (define (shift-n-and-mul l n x)
    (append (map (lambda (y) (mul x y)) l) (make-list n 0)))

  (define (add-terms t1 t2)
    (cond ((empty-termlist? t1) t2)
          ((empty-termlist? t2) t1)
          (else
           (let ((o1 (highest-order t1))
                 (o2 (highest-order t2))
                 (dl1 (sparse-list t1))
                 (dl2 (sparse-list t2)))
             (cond ((>= o1 o2)
                    (make-terms o1 (append-add dl1 (- o1 o2) dl2)))
                   ((< o1 o2)
                    (make-terms o2 (append-add dl2 (- o2 o1) dl1))))))))

  ;; (define (add-poly-poly t1 t2)
  ;;   (if ()))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (make-terms (+ (order t1) (highest-order L))
                    (shift-n-and-mul (sparse-list L) (order t1) (coeff t1)))))

  (define (make-terms h-order coeffs)
    (cond ((null? coeffs) (list 0 '()))
          ((=zero? (car coeffs)) (make-terms (- h-order 1) (cdr coeffs)))
          (else (list h-order coeffs))))

  (define (neg-terms term-list)
    (make-terms (highest-order term-list)
                (map neg (sparse-list term-list))))

  (define ftag-sparse (ftag 'sparse))

  (put-list '(sparse) ftag-sparse
            `((project ,identity)
              (raise ,identity)
              (neg ,neg-terms)))
  (put-list '(sparse sparse) ftag-sparse
            `((add ,add-terms)
              ;; (add-poly ,add-poly-poly)
              (mul ,mul-terms)
              ;; (div ,div-terms)
              ;; (mul-poly, mul-poly-poly)
              ))
  (put-list '(sparse) identity
            `((=zero ,empty-termlist?)))
  (put-list '(sparse sparse) identity
            `((equ? ,equ-terms?)))
  (put-list 'sparse ftag-sparse
            `((make-terms ,make-terms)))
  'done)
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cadr x))
  (define (make-rat n d)
    (if (=zero? d)
        (error "Zero in denominator")
        (reduce-arith n d)
        ;; (let ((g (greatest-common-divisor n d)))
        ;;   (basic-make-rat (div n g) (div d g)))
          ))


  (define (basic-make-rat n d)
    (list n d))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))

  (define ftag-rat (ftag 'rational))
  (define (gcd-rat a b)
    (greatest-common-divisor a b))
  (define (project-rat x)
    (if (eq? (type-tag (numer x)) 'scheme-number)
        (make-scheme-number (round (div (numer x) (denom x))))
        0))

  (put-list 'rational ftag-rat
            `((make ,make-rat)))
  (put-list 'rational identity
            `((make-1 ,(lambda (x)
                         (let ((rat_val
                                (rationalize (inexact->exact x) 1/1000000)))
                           (make-rational (numerator rat_val)
                                          (denominator rat_val)))))))
  (put-list '(rational rational) ftag-rat
            `((add ,add-rat)
              (mul ,mul-rat)
              (div ,div-rat)
              (sub ,sub-rat)
              (greatest-cd ,gcd-rat)))
  (put-list '(rational rational) identity
            `((equ? ,(lambda (x y) (and (equ? (numer x) (numer y))
                                        (equ? (denom x) (denom y)))))))
  (put-list '(rational) identity
            `((=zero? ,(lambda (x) (=zero? (numer x))))
              (raise ,(lambda (x) (make-complex-from-real-imag
                                   (div (numer x)
                                        (denom x)) 0)))
              (project ,project-rat)))

  ;; (define (rational-number->complex n)
  ;;    (make-complex-from-real-imag (/ (numer n) (denom n)) 0))

  (put-coercion 'rational 'complex
                (lambda (n) (make-complex-from-real-imag
                             (/ (numer (contents n))
                                (denom (contents n))) 0)))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))
(define (greatest-common-divisor a b)
  (apply-generic 'greatest-cd a b))
(define (reduce-arith n d)
    (apply-generic 'reduce-arith n d))
(install-rational-package)
(install-polynomial-package)
(install-dense-package)
(install-sparse-package)
(define (make-sparse-terms terms)
  ((get 'make-terms 'sparse) terms))

(define (make-poly var terms)
  ((get 'make 'polynomial) var terms))

(define make-dense-terms (get 'make-terms 'dense))

;; (define (equ-terms? t1 t2)
;;   (apply-generic 'equ-terms? t1 t2))

(define (neg-terms t1)
  (apply-generic 'neg t1))

(define (div p1 p2)
  (apply-generic 'div p1 p2))
