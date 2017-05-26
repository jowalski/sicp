(test-begin "rational arithmetic" 14)

;;; tests of arithmetic components from SICP
;;; 2017 Jowalski

;; The optional count must match the number of test-cases executed by this
;; group. (Nested test groups count as a single test case for this count.) This
;; extra test may be useful to catch cases where a test doesn't get executed
;; because of some unexpected error.
;;
;; (test-begin "arithmetic" 169)

(test-assert "make-rational" (pair? (make-rational 3 7)))

(test-equal "type rational"
  'rational (type-tag (make-rational 8 5)))

(test-equal "add rational"
  (make-rational 4 7)
  (add (make-rational 2 7) (make-rational 2 7)))

(test-equal "multiply rational"
  (make-rational 3 8)
  (mul (make-rational 1 4) (make-rational 3 2)))

(test-equal "subtract rational"
  (make-rational 2 9)
  (sub (make-rational 1 3) (make-rational 1 9)))

(test-equal "divide rational"
  (make-rational 3 1)
  (div (make-rational 21 1) (make-rational 7 1)))

(test-equal "reduce terms"
  (make-rational 2 3)
  (make-rational 14 21))

(test-assert "equal rational"
  (equ? (make-rational 3 11) (make-rational 3 11)))

;; by the formula, all zero numerators should reduce to 0 1
(test-assert "equal zero rational"
  (=zero? (make-rational 0 1)))
(test-assert "equal zero rational (0 2)"
  (=zero? (make-rational 0 2)))
(test-error "rational in denominator (error)"
            (make-rational 1 0))

(test-equal "project rational to scheme number"
  5 (project (make-rational 5 1)))

(test-equal "raise rational to complex number"
  (make-complex-from-real-imag 6 0)     ; (will be dropped to scheme-number)
  (raise (make-rational 6 1)))

(test-assert "make rational polynomial"
  (pair? (make-rational (make-poly '(x) (make-dense-terms '((2 1))))
                        (make-poly '(x) (make-dense-terms '((0 1)))))))

(test-end)
