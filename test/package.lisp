;;;; test/package.lisp - Test suite package for CLoak

(defpackage #:cloak.test
  (:use #:cl #:fiveam #:cloak.protocol #:cloak.buffer)
  (:export #:run-tests))

(in-package #:cloak.test)

(def-suite :cloak-tests
  :description "CLoak IRC bouncer test suite")

(defun run-tests ()
  (run! :cloak-tests))
