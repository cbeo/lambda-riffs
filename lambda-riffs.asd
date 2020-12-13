;;;; lambda-tools.asd

(asdf:defsystem #:lambda-riffs
  :description "Macros and utilities for higher-order riffing"
  :author "Colin Okay <okay@toyful.space>"
  :license  "GPLv3"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "macros")
               (:file "functions")))
