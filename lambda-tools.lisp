;;;; lambda-tools.lisp

(in-package #:lambda-tools)

(defun $or (&rest predicates)
  "Each argument in PREDICATES is a predicate function of one
argument. Returns a new predicate, call it P, that is the
disjunction of each of the PREDICATES.

The value of (P X) is the value of the first predicate Q in PREDICATES
such that (Q X) is non-NIL, or is NIL if none of the PREDICATES return
non-NIL.

That is, the disjuction of PREDICATES is short-circuiting. If any
PREDICATES have side effects, they will be executed only if each of
the preceding predicates in the list returned NIL."
  (labels ((disj (x preds)
             (if (null preds) nil
                 (or (function pred x)
                     (disj x (cdr preds))))))
    (lambda (x) (disj x predicates))))

(defun $and (&rest predicates)
  "Each argument in PREDICATES is a predicate function of one
argument. Returns a new predicate of one argument, call it P, that is
the conjunction of each of the PREDICATES.

The value of (P X) is NIL if any of the PREDICATES applied to X are
NIL. Otherwise it is the value of the last member in PREDICATES
applied to X.

That is, the conjunction of PREDICATES is short-circuiting.  If any
PREDICATES have side effects, they will be executed only if each of
the preceding predicates in the list returned non-NIL."
  (labels ((conj (x preds)
             (cond ((null preds) t)
                   ((null (cdr preds)) (funcall (car preds) x))
                   ((funcall (car preds) x)
                    (conj x (cdr preds))))))
    (lambda (x) (conj x predicates))))
