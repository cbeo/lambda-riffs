(in-package :lambda-tools)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;;; some fucntions for workign with substitution variables

  (defun substitute-var-p (symbol)
    (and (not (eql '$$ symbol))
         (symbolp symbol)
         (eq (elt (symbol-name symbol) 0)
             #\$)))

  (defun numeric-char-p (c)
    (and (alphanumericp c) (not (alpha-char-p c))))
  
  (defun numeric-var-p (symbol)
    (and (substitute-var-p symbol)
         (numeric-char-p
          (elt (symbol-name symbol) 1))))


  (defun numerically-before-p (a b)
    (apply #'<
           (mapcar (lambda (x) (parse-integer (symbol-name x) :start 1 :junk-allowed t))
                   (list a b)))))

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
 (#\y 3 #\f)

You can specify the order of the variable in the argument list by
nameing the variable with a number after the $ symbol. If you go this
route, all of your variables must be numbered.

E.g.

> (reduce ($$ (concatenate 'string $2-x #\Space $1-acc))
          (list \"one\" \"two\" \"three\")
          :initial-value \"zubas\")

 \"three two one zubas\"


Note that you can use any numbers, the arguments are sorted by < on
those numbers.

"
  (let  ((new-params (list))
         (numeric-params nil))
    (subst-if t (constantly nil) expr
              :key (lambda (x)
                     (when (substitute-var-p x)
                       (pushnew x new-params))
                     (when (numeric-var-p x)
                       (setf numeric-params t))
                     (when (eql '$$ x)
                       (error "$$ cannot be nested"))))
    (setf new-params
          (if numeric-params 
              (sort new-params #'numerically-before-p)
              (reverse new-params)))
    `(lambda ,new-params  ,expr)))



(defmacro conj (&rest preds)
  (let ((block-label (gensym)))
    `(let ((preds (list ,@preds)))
       (lambda (arg)
         (block ,block-label
           (unless preds (return-from ,block-label t))
           (let (acc)
             (dolist (p preds)
               (setf acc (funcall p arg))
               (unless acc (return-from ,block-label nil)))
             acc))))))

(defmacro disj (&rest preds)
  (let ((block-label (gensym)))
    `(let ((preds (list ,@preds)))
       (lambda (arg)
         (block ,block-label
           (unless preds (return-from ,block-label nil))
           (let (acc)
             (dolist (p preds)
               (setf acc (funcall p arg))
               (when acc (return-from ,block-label acc)))
             acc))))))

(defmacro make-lazy (form)
  (let ((run-p (gensym))
        (val (gensym)))
    `(let ((,run-p nil)
           (,val nil))
       (lambda ()
         (unless ,run-p
           (setf ,val ,form)
           (setf ,run-p t))
         ,val))))


(defun enable-partial-eval-reader-macro ()
  (set-dispatch-macro-character
   #\# #\$
   (lambda (stream subchar arg)
     (declare (ignore arg subchar))
     (list '$$ (read stream)))))


(defun enable-lazy-eval-reader-macros ()

  (set-dispatch-macro-character
   #\# #\~
   (lambda (stream subchar arg)
     (declare (ignore arg subchar))
     (list 'make-lazy (read stream))))

  (set-dispatch-macro-character
   #\# #\!
   (lambda (stream subchar arg)
     (declare (ignore arg subchar))
     (list 'funcall (read stream)))))
