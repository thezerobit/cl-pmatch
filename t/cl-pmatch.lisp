#|
  This file is a part of cl-pmatch project.
  Copyright (c) 2012 Stephen A. Goss (steveth45@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-pmatch-test
  (:use :cl
        :cl-pmatch
        :cl-test-more))
(in-package :cl-pmatch-test)

(plan 31)

(defparameter *test-matches*
  `(
    ;; symbol matching
    ((a b c) (a b c) ,*success*)
    ((a b c) (a b d) nil)
    ;; 'or matching
    ((a b c (or d e)) (a b c d) ,*success*)
    ((a b c (or d e)) (a b c e) ,*success*)
    ((a b c (or d e)) (a b c f) nil)
    ;; '? (maybe) matching
    ((a b (? c) d) (a b c d) ,*success*)
    ((a b (? c d e) d) (a b d) ,*success*)
    ((a b (? c) d) (a b e d) nil)
    ((a b (? c d e) d) (a b c d e d) ,*success*)
    ;; mixed 'or '? matching
    ((a (? (or b c)) d) (a b d) ,*success*)
    ((a (? (or b c)) d) (a c d) ,*success*)
    ((a (? (or b c)) d) (a d) ,*success*)
    ((a (? (or b c)) d) (a b c d) nil)
    ((a (? (or b c)) d) (a e d) nil)
    ;; '+ matching
    ((a (+ b) c) (a c) nil)
    ((a (+ b) c) (a b c) ,*success*)
    ((a (+ b d) c) (a b d b d c) ,*success*)
    ;; '* matching
    ((a (* b) c) (a c) ,*success*)
    ((a (* b) c) (a b c) ,*success*)
    ((a (* b d) c) (a b d b d c) ,*success*)
    ;; named subgroup
    ((a (:middle b (? c)) d) (a b c d) ((:middle b c)))
    ((a (:middle b (? c)) d) (a b d) ((:middle b)))
    ((a (:middle b (or c e)) d) (a b c d) ((:middle b c)))
    ((a (:middle b (or c e)) d) (a b e d) ((:middle b e)))
    ;; any (type) defaults to T which matches all types
    ((a (any) c) (a b c) ,*success*)
    ((a (any symbol) c) (a 100 c) nil)
    ((a (any integer) c) (a 100 c) ,*success*)
    ))

(dolist (test *test-matches*)
  (destructuring-bind (p s r) test
    (let ((result (pmatch p s)))
      (ok (equal r result) 
          (format nil "~a should match ~a for~% (pmatch ~a ~a)" result r p s)))))

(let ((tests
        `(
          ((a b (:foo c d) e f)
           (a b (cl-pmatch:push-group :foo) c d (cl-pmatch:pop-group :foo) e f))
          ((a b (:foo c d) (or (:bar e) f))
           (a b (cl-pmatch:push-group :foo) c d (cl-pmatch:pop-group :foo)
              (or (cl-pmatch:push-group :bar) e (cl-pmatch:pop-group :bar) f)))
          ((a b c) (a b c))
          )))
  (dolist (test tests)
    (destructuring-bind (input output) test
      (if (not (equal output (flatten-groups input)))
        (fail (format nil "FLATTEN-GROUPS fail: ~a does not match ~a"
                (flatten-groups input) output))
        (pass "FLATTEN-GROUPS passed.")))))

(defmacro -> (x &optional (form nil form-supplied-p) &rest more)
  "Threading macro borrowed from Clojure."
  (if form-supplied-p
    (if more
      `(-> (-> ,x ,form) ,@more)
      (if (listp form)
        `(,(car form) ,x ,@(cdr form))
        (list form x)))
    x))

(let ((result-1
        (-> (make-gc)
            (gc-add-value 1)
            (gc-push-group :foo)
            (gc-add-value 2)
            (gc-push-group :bar)
            (gc-add-value 3)
            (gc-add-value 4)
            (gc-pop-group :bar)
            (gc-add-value 5)
            (gc-pop-group :foo)
            (gc-add-value 6)
            (gc-get-groups)
            ))
      (expected-1 `((:bar 3 4) (:foo 2 3 4 5))))
  (if (not (equal result-1 expected-1))
    (fail (format nil "GC FAIL: ~a does not equal ~a~%" result-1 expected-1))
    (pass "GC Passed.")))

(finalize)
