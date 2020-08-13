
(defpackage #:lt-examples
  (:use #:cl #:lambda-tools))


;; http://rosettacode.org/wiki/Luhn_test_of_credit_card_numbers 
(defun luhn (n)
  (>> n #'reverse
      #$(map 'list #'digit-char-p $char)
      #$(mapcar #'*
                (loop :for i :upto (length $digits) :collect (1+ (mod i 2)))
                $digits)
      #$(mapcar #$$(if (> $$x 9) (- $$x 9) $$x) $digits)
      #$(zerop (mod (apply #'+ $digits) 10))))

