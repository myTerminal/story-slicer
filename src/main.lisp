;;;; -------------------------------------------------------------------------
;;;; main

(in-package :main)

(defun validate-inputs (arguments)
  "Validates whether an input file was specified."
  (unless (car arguments)
    (progn
      (log-to-stdout "Please specify an input file!")
      (uiop:quit)))
  (unless (cadr arguments)
    (progn
      (log-to-stdout "Please specify an output directory!")
      (uiop:quit))))

(defun main ()
  "The main entry point to the program."
  (let ((args (uiop:command-line-arguments)))
    ;; Run all validations on input arguments
    (validate-inputs args)

    (let* ((input-filename (car args))
           (output-directory (cadr args))
           (max-length (if (caddr args)
                           (parse-integer (caddr args) :junk-allowed t)
                           30))
           (start-offset (if (cadddr args)
                             (parse-integer (cadddr args) :junk-allowed t)
                             0))
           (length-of-video (if (fifth args)
                                (parse-integer (fifth args) :junk-allowed t)
                                (parse-string-to-float (get-result-from-system (concatenate 'string
                                                                                            "ffprobe -i "
                                                                                            input-filename
                                                                                            " -show_entries format=duration -v quiet -of csv=\"p=0\""))))
             )
           (number-of-slices (ceiling (/ (- length-of-video start-offset)
                                         max-length))))
      (loop for i from 1 to number-of-slices
            do (let* ((start-seconds (+ (* (1- i) max-length) start-offset))
                      (length-in-seconds (if (> (+ start-seconds max-length)
                                                length-of-video)
                                             (- length-of-video start-seconds)
                                             max-length)))
                 (log-to-stdout (concatenate 'string
                                             (write-to-string length-in-seconds)
                                             " from "
                                             (write-to-string start-seconds)))
                 (execute-in-system (concatenate 'string
                                                 "ffmpeg -ss "
                                                 (write-to-string start-seconds)
                                                 " -i "
                                                 input-filename
                                                 " -t "
                                                 (write-to-string length-in-seconds)
                                                 " -c copy "
                                                 output-directory
                                                 "/slice_"
                                                 (write-to-string i)
                                                 ".mp4"))))
      (log-to-stdout (concatenate 'string
                                  "File split into "
                                  (write-to-string number-of-slices)
                                  " slices.")))))
