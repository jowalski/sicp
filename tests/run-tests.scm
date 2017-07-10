#!/usr/bin/guile
!#

(load "../arithmetic.scm")

(load "test-runner.scm")

(test-runner-factory
 (lambda () (my-simple-runner "tests.log")))

(load "test-arithmetic.scm")
(load "test-poly.scm")
