;;;; lambda-tools.lisp

(in-package #:lambda-riffs)

(defun -> (arg &rest fns)
  (dolist (fn fns arg)
    (setf arg (funcall fn arg))))

(defun all> (arg &rest preds)
  "Predicate Filter. Returns ARG if (PRED ARG) is non-NIL for each
PRED in PREDS"
  (dolist (pred preds arg)
    (unless (funcall pred arg)
      (return-from all> nil))))


(defun some> (arg &rest preds)
  (dolist (pred preds nil)
    (when (funcall pred arg)
      (return-from some> arg))))


(defun <> (&rest fns)
  (lambda (arg)
    (apply #'>> arg fns)))



