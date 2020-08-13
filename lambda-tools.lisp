;;;; lambda-tools.lisp

(in-package #:lambda-tools)

(defun >> (arg &rest fns)
  (dolist (fn fns)
    (setf arg (funcall fn arg)))
  arg)


(defun <> (&rest fns)
  (lambda (arg)
    (apply #'>> arg fns)))



