(defpackage :utils
  (:use :cl)
  (:export :parse-string-to-float))

(defpackage :shell
  (:use :cl)
  (:export :log-to-stdout
           :execute-in-system
           :get-result-from-system))

(defpackage :main
  (:use :cl)
  (:import-from :utils
                :parse-string-to-float)
  (:import-from :shell
                :log-to-stdout
                :execute-in-system
                :get-result-from-system)
  (:export :main))
