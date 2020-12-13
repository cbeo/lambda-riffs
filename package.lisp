;;;; package.lisp

(defpackage #:lambda-riffs
  (:use #:cl)
  (:export #:$
           #:->
           #:all>
           #:some>
           #:make-lazy
           #:conj
           #:disj))
