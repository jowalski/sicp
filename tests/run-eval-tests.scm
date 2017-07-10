#!/usr/bin/guile
!#

(load "../evaluator.scm")

(load "test-runner.scm")

(test-runner-factory
 (lambda () (my-simple-runner "evaluator-tests.log")))

(load "test-evaluator.scm")
