;;;; -------------------------------------------------------------------------
;;;; Common utility helpers

(in-package :utils)

(defun parse-string-to-float (string)
  "Converts a string into a number."
  (car (let ((*read-eval* nil))
         (with-input-from-string (stream string)
           (loop for number = (read stream nil nil)
                 while number collect number)))))
