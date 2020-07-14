(in-package :lambda-tools)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun is-substitute-var (symbol)
    (and (not (eql '$$ symbol))
         (symbolp symbol)
         (eq (elt (symbol-name symbol) 0)
             #\$))))





(defmacro $$ (expr)
  "Quickly create functions from an expression EXPR with 'blanks' in
it. Each blank is a symbol that betins with the underscore _. Symbols
with the same name are treated as the same variable.

A function is returned, it accepts exactly the number of variables as
there were unique blanks in the expression. When calling the new
function, the variables are bound in the order they appeared in EXPR.

This is a rather simple macro - you cannot nest $$ forms. If you try,
an error will be raised.

> (macroexpand-1 '($$ (+ $a (* $b 3) $b (- $a $c) 10)))
  (LAMBDA ($A $B $C) 
     (+ $A 
       (* $B 3) 
       $B 
       (- $A $C) 
       10))

The macro is useful for succinctly passing functions to
higher order functions:

> (mapcar ($$ (+ $ 10)) '(1 2 3 4))
 (11 12 13 14)

> (let ((elt-num 2))
    (mapcar ($ (elt $ elt-num)) 
            (list \"hey dude\" 
                  #(1 2 3 4)
                  \"ffffffffff\")))
 (#\y 3 #\f)"
  (let  ((new-params (list)))
    (subst-if t (constantly nil) expr
              :key (lambda (x)
                     (when (is-substitute-var x)
                       (pushnew x new-params))
                     (when (eql '$$ x)
                       (error "$$ cannot be nested"))))
    `(lambda ,(reverse  new-params) ,expr)))






