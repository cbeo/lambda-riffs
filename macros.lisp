(in-package :lambda-tools)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun is-substitute-var (symbol)
    (and (symbolp symbol)
         (eq (elt (symbol-name symbol) 0)
             #\_))))





(defmacro $ (expr)
"Quickly create functions from an expression EXPR with 'blanks' in
it. Each blank is a symbol that betins with the underscore _. Symbols
with the same name are treated as the same variable.

A function is returned, it accepts exactly the number of variables as
there were unique blanks in the expression. When calling the new
function, the variables are bound in the order they appeared in EXPR.

> (macroexpand-1 '($ (+ _a (* _b 3) _b (- _a _c) 10)))
  (LAMBDA (_A _B _C) 
     (+ _A 
       (* _B 3) 
       _B 
       (- _A _C) 
       10))

The macro is useful for succinctly passing functions to
higher order functions:

> (mapcar ($ (+ _ 10)) '(1 2 3 4))
 (11 12 13 14)

> (let ((elt-num 2))
    (mapcar ($ (elt _ elt-num)) 
            (list \"hey dude\" 
                  #(1 2 3 4)
                  \"ffffffffff\")))
 (#\y 3 #\f)
"
  (let  ((new-params (list)))
    (subst-if t (constantly nil) expr
              :key (lambda (x) (when (is-substitute-var x)
                                 (pushnew x new-params))))
    `(lambda ,(reverse  new-params) ,expr)))






