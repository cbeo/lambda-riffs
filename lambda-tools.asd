;;;; lambda-tools.asd

(asdf:defsystem #:lambda-tools
  :description "Macros and utilities for combining making and lambdas"
  :author "Colin Okay <cbeok@protonmail.com>"
  :license  "GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (:alexandria)
  :components ((:file "package")
               (:file "macros")
               (:file "lambda-tools")))
