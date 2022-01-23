(defpackage :bamboo-trim
  (:use :cl)
  (:export #:make-sim
           #:run-sim
           #:reduce-max
           #:history
           #:state-iter
           #:state-heights
           #:state-choice
           #:analysis
           #:analysis-max-height
           #:analysis-max-height-iter
           #:analysis-max-bamboo
           #:sim-tofile))
