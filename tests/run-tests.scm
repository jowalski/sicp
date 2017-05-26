#!/usr/bin/guile
!#

(use-modules (srfi srfi-64)
             (ice-9 format))

(load "../arithmetic.scm")

(define (my-simple-runner filename)
  (let ((runner (test-runner-null))
        (port (open-output-file filename))
        (num-passed 0)
        (num-failed 0))
    (test-runner-on-group-begin! runner
      (lambda (runner name cnt)
        (format port "Testing group: ~a, ~d stated tests~%"
                name (or cnt 0))))
    (test-runner-on-test-end! runner
      (lambda (runner)
        (case (test-result-kind runner)
          ((pass xpass) (set! num-passed (+ num-passed 1)))
          ((fail xfail) (set! num-failed (+ num-failed 1))
           (format port "FAIL: ~a (expected: ~a, actual: ~a)~%"
                   (test-runner-test-name runner)
                   (test-result-ref runner 'expected-value)
                   (test-result-ref runner 'actual-value)))
          (else #t))))
    (test-runner-on-final! runner
      (lambda (runner)
        (format port "Passing tests: ~d.~%Failing tests: ~d.~%"
                num-passed num-failed)
        (set! num-passed 0)
        (set! num-failed 0)
        ;; (close-output-port port)
        ))
    runner))

(test-runner-factory
 (lambda () (my-simple-runner "tests.log")))

(load "test-arithmetic.scm")
(load "test-poly.scm")
