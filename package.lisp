;;;; package.lisp

(defpackage #:lambda-tools
  (:use #:cl)
  (:export #:$
           #:>>
           #:<>
           #:conj
           #:disj
           #:enable-partial-eval-reader-macro
           #:enable-lazy-eval-reader-macros))
