(test-begin "polynomial arithmetic" 19)

(define (make-simple-poly pairs . l)
  (make-poly '(x) (make-dense-terms pairs)))

(define (make-poly-w-rdr pairs rdr-pairs)
  (make-poly '(x) (make-dense-terms pairs rdr-pairs)))

(define (make-poly-w-var vars pairs)
  (make-poly vars (make-dense-terms pairs)))


;; test basic polynomial with scheme-numbers
(test-assert "make polynomial"
  (pair? (make-simple-poly '((2 1)))))

(test-equal "type polynomial"
  'polynomial (type-tag (make-simple-poly '((2 3)))))

;; arithmetic on same variable
(test-equal "add polynomial"
  (make-simple-poly '((3 2) (2 1) (0 3)))
  (add (make-simple-poly '((3 1) (0 2)))
       (make-simple-poly '((3 1) (2 1) (0 1)))))

(test-equal "multiply polynomial"
  (make-simple-poly '((5 6) (4 2) (3 15) (2 3) (1 9)))
  (mul (make-simple-poly '((3 2) (1 3)))
       (make-simple-poly '((2 3) (1 1) (0 3)))))

(test-equal "subtract polynomial"
  (make-simple-poly '((3 2) (2 1) (1 4) (0 -3)))
  (sub (make-simple-poly '((3 2) (2 4) (1 5)))
       (make-simple-poly '((2 3) (1 1) (0 3)))))

(test-equal "divide polynomial"
  (make-poly-w-rdr '((1 2/3) (0 10/9)) '((1 17/9) (0 -10/3)))
  (div (make-simple-poly '((3 2) (2 4) (1 5)))
       (make-simple-poly '((2 3) (1 1) (0 3)))))


;; arithmetic on different variables
(test-equal "multiply polynomial (multiple variables)"
  (make-poly-w-var '(x y) `((3 ,(make-dense-terms '((2 8) (0 12))))
                            (2 ,(make-dense-terms '((2 2) (0 3))))
                            (1 ,(make-dense-terms '((2 4) (0 6))))))
  (mul (make-poly-w-var '(x) '((3 4) (2 1) (1 2)))
       (make-poly-w-var '(y) '((2 2) (0 3)))))

(test-equal "add polynomial (multiple variables)"
  (make-poly-w-var '(x y) `((3 4) (2 1) (1 2)
                            (0 ,(make-dense-terms '((2 2) (0 3))))))
  (add (make-poly-w-var '(x) '((3 4) (2 1) (1 2)))
       (make-poly-w-var '(y) '((2 2) (0 3)))))

(test-equal "gcd polynomial"
  (make-simple-poly '((2 -1) (1 1)))
  (greatest-common-divisor (make-simple-poly '((4 1) (3 -1) (2 -2) (1 2)))
                           (make-simple-poly '((3 1) (1 -1)))))

(test-error "gcd polynomial different vars (error)"
            (greatest-common-divisor (make-simple-poly '((4 1) (3 -1) (2 -2) (1 2)))
                                     (make-poly-w-var '(y) '((3 1) (1 -1)))))

(let ((P_1 (make-simple-poly '((2 1) (1 -2) (0 1))))
      (P_2 (make-simple-poly '((2 11) (0 7))))
      (P_3 (make-simple-poly '((1 13) (0 5)))))
  (let ((Q_1 (mul P_1 P_2))
        (Q_2 (mul P_1 P_3)))
    (test-equal "gcd reduce and simplify polynomial coefficients to integers"
      P_1
      (greatest-common-divisor Q_1 Q_2))
    (test-equal "reduce-arith on polynomial"
      (list P_2 P_3)
      (reduce-arith Q_1 Q_2))))

(let   ((p1 (make-simple-poly '((1 1) (0 1))))
        (p2 (make-simple-poly '((3 1) (0 -1))))
        (p3 (make-simple-poly '((1 1))))
        (p4 (make-simple-poly '((2 1) (0 -1)))))
  (let ((rf1 (make-rational p1 p2))
        (rf2 (make-rational p3 p4)))
    (test-equal "reduce rational polynomials automatically"
    (make-rational (make-simple-poly '((3 -1) (2 -2) (1 -3) (0 -1)))
                   (make-simple-poly '((4 -1) (3 -1) (1 1) (0 1))))
    (add rf1 rf2))))

(test-equal "reduce rational integers automatically"
  (make-rational 5 2)
  (add (make-rational 2 3) (make-rational 11 6)))
;; ...

(test-assert "equal simple polynomial"
  (equ? (make-simple-poly '((3 11)))
        (make-simple-poly '((3 11)))))

(test-assert "equal polynomial with remainder"
  (equ? (make-poly-w-rdr '((3 11)) '((1 3) (0 9)))
        (make-poly-w-rdr '((3 11)) '((1 3) (0 9)))))

(test-assert "equal zero polynomial"
  (=zero? (make-simple-poly '())))

(test-equal "project polynomial"
  (make-simple-poly '((2 1) (1 2)))
  (project (make-simple-poly '((2 1) (1 2)))))

(test-equal "raise polynomial"
  (make-simple-poly '((2 1) (1 2)))
  (raise (make-simple-poly '((2 1) (1 2)))))

;; test basic sparse polynomial
;; ...

;; test with non-scheme numbers (rational, complex)
;; ...

(test-end)
