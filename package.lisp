;;;; package.lisp

(defpackage #:lambda-tools
  (:use #:cl)
  (:export #:$$
           #:>>
           #:<>
           #:and>
           #:or>
           #:enable-partial-eval-reader-macro
           #:enable-lazy-eval-reader-macros))
