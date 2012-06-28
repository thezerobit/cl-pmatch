#|
  This file is a part of cl-pmatch project.
  Copyright (c) 2012 Stephen A. Goss (steveth45@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-pmatch
  (:use :cl)
  (:export :pmatch
           :pmatch-list
           :pmatch-aux
           :pmatch-internal
           :compile-pattern
           :or
           :?
           :+
           :*
           :any
           :list
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

(defclass compiled-pattern ()
  ((cpattern :initarg :cpattern
             :reader cpattern)))

(defun compile-pattern (pattern)
  (make-instance 'compiled-pattern :cpattern (flatten-groups pattern)))

;; match the pattern (either a list or compiled-pattern) against
;; an input list
(defgeneric pmatch (pattern input))

;; specialize on pattern rules that are lists starting with a symbol
;; '(foo my precious) -> (pmatch-list 'foo ('foo my precious) ....)
(defgeneric pmatch-list (sym rule pattern input gc))

;; specialize on rule type, builtin handlers exist for symbols and lists
(defgeneric pmatch-aux (rule pattern input gc))

(defmethod pmatch ((pattern list) input)
  (pmatch-internal (flatten-groups pattern) input (make-gc)))

(defmethod pmatch ((pattern compiled-pattern) input)
  (pmatch-internal (cpattern pattern) input (make-gc)))

(defun pmatch-internal (pattern input gc)
  (if (null pattern)
    (and (null input) (or (gc-get-groups gc) *success*))
    (pmatch-aux (car pattern) (cdr pattern) input gc)))

(defmethod pmatch-aux ((rule symbol) pattern input gc)
  (and (not (null input))
       (eql rule (car input))
       (pmatch-internal pattern (cdr input)
                        (gc-add-value gc (car input)))))

(defmethod pmatch-aux ((rule list) pattern input gc)
  (pmatch-list (car rule) rule pattern input gc))

(defmethod pmatch-list ((sym (eql 'or)) rule pattern input gc)
  (dolist (option (cdr rule))
    (let ((result (pmatch-internal (cons option pattern)
                                   input gc)))
      (if result (return-from pmatch-list result)))))

(defmethod pmatch-list ((sym (eql '?)) rule pattern input gc)
  (or (pmatch-internal (append (cdr rule) pattern) input gc)
      (pmatch-internal pattern input gc)))

(defmethod pmatch-list ((sym (eql '+)) rule pattern input gc)
  (let ((maybe (cdr rule)))
    (or (and (not (null input))
             (pmatch-internal (append maybe (cons rule pattern)) input gc))
        (pmatch-internal (append maybe pattern) input gc))))

(defmethod pmatch-list ((sym (eql '*)) rule pattern input gc)
  (let ((maybe (cdr rule)))
    (or (and (not (null input))
             (pmatch-internal (append maybe (cons rule pattern)) input gc))
        (pmatch-internal pattern input gc))))

(defmethod pmatch-list ((sym (eql 'any)) rule pattern input gc)
  (when (typep (car input) (or (cadr rule) T))
    (pmatch-internal pattern (cdr input)
                     (gc-add-value gc (car input)))))

(defmethod pmatch-list ((sym (eql 'list)) rule pattern input gc)
  (pmatch-internal (append (cdr rule) pattern) input gc))

(defmethod pmatch-list ((sym (eql 'push-group)) rule pattern input gc)
  (pmatch-internal pattern input (gc-push-group gc (cadr rule))))

(defmethod pmatch-list ((sym (eql 'pop-group)) rule pattern input gc)
  (pmatch-internal pattern input (gc-pop-group gc (cadr rule))))
