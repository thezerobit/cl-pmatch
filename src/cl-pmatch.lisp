#|
  This file is a part of cl-pmatch project.
  Copyright (c) 2012 Stephen A. Goss (steveth45@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-pmatch
  (:use :cl)
  (:export :pmatch
           :or
           :?
           ;; the rest of these are for the test framework
           :*success*
           :flatten-groups
           :make-gc
           :gc-add-value
           :gc-push-group
           :gc-pop-group
           :gc-get-groups
           :push-group
           :pop-group))
(in-package :cl-pmatch)

(defparameter *success* T)

(defun flatten-groups (pattern)
  (let ((new-list '()))
    (loop while pattern
          do (let ((next (pop pattern)))
               (cond
                 ((listp next) (cond
                                 ((keywordp (car next))
                                  (push (list 'push-group (car next)) new-list)
                                  (dolist (elem (flatten-groups (cdr next)))
                                    (push elem new-list))
                                  (push (list 'pop-group (car next)) new-list))
                                 (T (push (flatten-groups next) new-list))))
                 (T (push next new-list)))))
    (nreverse new-list)))

(defun make-gc ()
  (cons '() '()))

(defun gc-push-group (gc groupname)
  (cons (cons (list groupname) (car gc))
        (cdr gc)))

(defun gc-add-value (gc value)
  (let ((groups (car gc))
        (new-groups '()))
    (dolist (group groups)
      (push (cons value group) new-groups))
    (cons (nreverse new-groups) (cdr gc))))

(defun gc-pop-group (gc groupname)
  (let* ((groups (car gc))
         (first-group-reverse (pop groups))
         (group (reverse first-group-reverse)))
    (assert (eql (car group) groupname))
    (cons groups (cons group (cdr gc)))))

(defun gc-get-groups (gc)
  (reverse (cdr gc)))

(defun pmatch (pattern input)
  (pmatch-internal (flatten-groups pattern) input (make-gc)))

(defun pmatch-internal (pattern input gc)
  (if (null pattern)
    ;; null pattern
    (and (null input) (or (gc-get-groups gc) *success*))
    ;; not null pattern
    (let ((p1 (car pattern)))
      (if (symbolp p1)
        ;; first in pattern is symbol
        (and (eql p1 (car input))
             (pmatch-internal (cdr pattern) (cdr input)
                              (gc-add-value gc (car input))))
        ;; first in pattern is something else
        (pmatch-aux p1 (cdr pattern) input gc)))))

;; anything that's not a symbol or a list
;; can have a method, allowing for user-defined
;; matching classes
(defgeneric pmatch-aux (rule pattern input gc))

(defmethod pmatch-aux ((rule list) pattern input gc)
  (let ((r1 (car rule))
        (rest-rule (cdr rule)))
    (cond
      ((eq r1 'or) (pmatch-or rest-rule pattern input gc))
      ((eq r1 '?) (pmatch-? rest-rule pattern input gc))
      ((eq r1 'push-group) (push-group rest-rule pattern input gc))
      ((eq r1 'pop-group) (pop-group rest-rule pattern input gc))
      (T nil) ;; unknown == fail
      )))

(defun pmatch-or (options pattern input gc)
  (dolist (option options)
    (let ((result (pmatch-internal (cons option pattern)
                                   input gc)))
      (if result (return-from pmatch-or result)))))

(defun pmatch-? (maybe pattern input gc)
  (or (pmatch-internal (cons (car maybe) pattern) input gc)
      (pmatch-internal pattern input gc)))

(defun push-group (rest-rule pattern input gc)
  (pmatch-internal pattern input (gc-push-group gc (car rest-rule))))

(defun pop-group (rest-rule pattern input gc)
  (pmatch-internal pattern input (gc-pop-group gc (car rest-rule))))
