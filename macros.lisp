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
  (let ((run-p (gensym))
        (val (gensym)))
    `(let ((,run-p nil)
           (,val nil))
       (lambda ()
         (unless ,run-p
           (setf ,val ,form)
           (setf ,run-p t))
         ,val))))



