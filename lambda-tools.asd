;;;; lambda-tools.asd

(asdf:defsystem #:lambda-tools
  :description "Describe lambda-tools here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:alexandria)
  :components ((:file "package")
               (:file "macros")
               (:file "lambda-tools")))
