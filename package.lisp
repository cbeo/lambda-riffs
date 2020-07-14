;;;; package.lisp

(defpackage #:lambda-tools
  (:use #:cl)
  (:import-from #:alexandria
                #:if-let)
  (:export #:$$
           #:$and
           #:$or))
