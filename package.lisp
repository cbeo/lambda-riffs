;;;; package.lisp

(defpackage #:lambda-riffs
  (:use #:cl)
  (:export #:$
           #:->
           #:monadic>
           #:all>
           #:some>
           #:make-lazy
           #:conj
           #:disj))
