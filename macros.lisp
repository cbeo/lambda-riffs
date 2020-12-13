(in-package :lambda-riffs)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;;; some fucntions for workign with substitution variables

  (defun substitute-var-p (symbol prefix)
    (and (not (eql '$ symbol))
         (symbolp symbol)
         (<= (length prefix)
             (length (symbol-name symbol)))
         (string-equal (symbol-name symbol) prefix
                       :end1 (length prefix))))

  (defun numeric-var-p (symbol prefix)
    (and (substitute-var-p symbol prefix)
         (digit-char-p
          (elt (symbol-name symbol) (length prefix)))))


  (set-dispatch-macro-character
   #\# #\$
   (lambda (stream subchar infix)
     (declare (ignore subchar infix))
     (let ((form1 (read stream)))
       (if (symbolp form1)
           (list '$ (list (concatenate 'string "$"
                                       (symbol-name form1)))
                 (read stream))
           (list '$ () form1)))))
  
  
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

;; Note, presently references to upper level variables in nested
;; partials requires tha tthose upper level variables acttually appear
;; in the upper level partials.

;; e.g. 
;; 
;; #$(mapcar #$$(cons $$x (length $xs)) $xs)
;;
;;  is OK but 
;;
;; #$(mapcar #$$(cons $$x (length $passed-in-list)) '(1 2 3 4))
;;
;; is not ok. 

(defmacro $ ((&optional (prefix "$")) expr)
  "Function splicer. A kind of partial evaluation.

Meant to be used in conjunction with the reader macro #$.

E.g. 

#$(+ $X 1)  

is roughly equivalent to 

(LAMBDA ($X) (+ $X 1))

The order of arguments can be controlled by using positional
variables. E.g.

#$(+ $2 $1) 

is equivalent to 

(LAMBDA ($1 $2) (+ $2 $1))

Limited nestiing is supported.  E.g.

#$(MAPCAR #$$(CONS $$INNER (LENGTH $OUTER))  $OUTER) 

is equvalent to

(LAMBDA ($OUTER) 
  (MAPCAR (LAMBDA ($$INNER) (CONS $$INNER (LENGTH $OUTER)))
          $OUTER))

However, a variable inside a nested form must actually appear in the
surrounding form. 

THIS WONT WORK: #$(+ #$$(* $X $$Y)) because $$Y doesn't appear in the
surrounding form.

 "
  (let ((new-params (list))
        (numeric-params nil))
    (labels ((walk (node)
               (cond ((and
                       (consp node)
                       (consp (car node))
                       (eq '$ (caar node)))
                      (walk (cdr node)))
                     
                     ((consp node)
                      (walk (car node))
                      (walk (cdr node)))
                     (t
                      (when (substitute-var-p node prefix)
                        (pushnew node new-params))
                      (when (numeric-var-p node prefix)
                        (setf numeric-params t))))))
      (walk expr))
    (setf new-params
          (if numeric-params
              (sort new-params #'<
                    :key (lambda (var)
                           (parse-integer (symbol-name var)
                                          :junk-allowed t
                                          :start (length prefix))))
              (reverse new-params)))
    `(lambda ,new-params ,expr)))


(defmacro conj (&rest preds)
  "A composition macro. Short circuiting predicate conjunction."
  (let ((block-label (gensym)))
    `(let ((preds (list ,@preds)))
       (lambda (arg)
         (block ,block-label
           (unless preds (return-from ,block-label t))
           (let (acc)
             (dolist (p preds acc)
               (setf acc (funcall p arg))
               (unless acc (return-from ,block-label nil)))))))))


(defmacro disj (&rest preds)
  "A composition macro. Short circuiting predicate disjunction."
  (let ((block-label (gensym)))
    `(let ((preds (list ,@preds)))
       (lambda (arg)
         (block ,block-label
           (unless preds (return-from ,block-label nil))
           (let (acc)
             (dolist (p preds acc)
               (setf acc (funcall p arg))
               (when acc (return-from ,block-label acc)))))))))

(defmacro make-lazy (form)
  "Wraps FORM in a thunk.  Intended to be used with teh #~ and #! reader macros:

(let ((computation #~(progn (print 'hey) 10)))
  (cons #!computation #!computation))

HEY
(10 . 10)

The first time the computation is forced, it is run, and HEY is
printed. But the next time only the return value is used.
"
  (let ((run-p (gensym))
        (val (gensym)))
    `(let ((,run-p nil)
           (,val nil))
       (lambda ()
         (unless ,run-p
           (setf ,val ,form)
           (setf ,run-p t))
         ,val))))





(defmacro binding> ((&key (exit-when #'null) exit-value exit-function) init &rest functions)
  "A threading macro. Some examples:

(binding> ()
 \"hey dude what's the big idea\"       ; starting state
 #$(values (search \"the\" $s) $s)      ; multiple-values are passed along as arguments 
 #$(subseq $2 $1))                      : returns the result of the last form

should return \"the big idea\"

(binding> (:exit-with \"☹\")
    \"hey dude what's the big idea?\" 
    #$(values (search \"NOOOOOOPE\" $s) $s)
    #$(subseq $2 $1))

should return (\"☹\")

EXIT-WHEN should be a function, a predicate, that operates on the
first value returned from one of the forms.  If EXIT-WHEN returns
non-NIL, then the form exits early.

If EXIT-FUNCTION is non-NIL it is expected to be a function that
accepts a list, the list of values returned from the last function in
the chain.

If EXIT-FUNCTION is NIL, then EXIT-VALUE is returned instead.

e.g 

(binding> (:exit-when #'evenp 
           :exit-function #$(list* :failed $x))
     33
     #$(+ 11 $x)
     #$(- 5 $x))

will return  (:FAILED 44)

The default value of EXIT-WHEN is the predicate NULL.

"
  (let ((vals (gensym))
        (fn (gensym))
        (block-label (gensym)))
    `(let ((,vals (multiple-value-list ,init)))
       (block ,block-label 
         (dolist (,fn (list ,@functions) (values-list ,vals))
           (setq ,vals  (multiple-value-list (apply ,fn ,vals)))
           (when (funcall ,exit-when (car ,vals))
             (return-from ,block-label
               (if ,exit-function (funcall ,exit-function ,vals)
                   ,exit-value))))))))



