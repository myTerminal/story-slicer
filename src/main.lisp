;;;; -------------------------------------------------------------------------
;;;; main

(in-package :main)

(defun main ()
  "The main entry point to the program."
  (let ((args (uiop:command-line-arguments)))
    (unless (car args)
      (progn
        (log-to-stdout "Please specify an input file!")
        (uiop:quit)))
    (unless (cadr args)
      (progn
        (log-to-stdout "Please specify an output directory!")
        (uiop:quit)))
    (let* ((filename (car args))
           (output-directory (cadr args))
           (max-length (if (caddr args)
                           (parse-integer (caddr args) :junk-allowed t)
                           30))
           (start-offset (if (cadddr args)
                             (parse-integer (cadddr args) :junk-allowed t)
                             0))
           (length-of-video (parse-string-to-float (get-result-from-system (concatenate 'string
                                                                                        "ffprobe -i "
                                                                                        filename
                                                                                        " -show_entries format=duration -v quiet -of csv=\"p=0\""))))
           (number-of-slices (ceiling (/ (- length-of-video start-offset)
                                         max-length))))
      (loop for i from 1 to number-of-slices
            do (execute-in-system (concatenate 'string
                                               "ffmpeg -ss "
                                               (write-to-string (+ (* (1- i) max-length) start-offset))
                                               " -i "
                                               filename
                                               " -t "
                                               (write-to-string max-length)
                                               " -c copy "
                                               output-directory
                                               "/slice_"
                                               (write-to-string i)
                                               ".mp4")))
      (log-to-stdout (concatenate 'string
                                  "File split into "
                                  (write-to-string number-of-slices)
                                  " slices.")))))
