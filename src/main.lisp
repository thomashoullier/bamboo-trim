;;;; Using bamboo-trim
(asdf:load-system "bamboo-trim")

;; Working data directory
(defparameter *system-dir* (asdf:system-source-directory "bamboo-trim"))
(defparameter *data-dir* (merge-pathnames #P"data/" *system-dir*))

;;; sim1: Minimal example: two bamboos with reduce-max for 100 iterations.
;;;       Illustating optimum lower bound of 2.
(defparameter *sim1*
  (bamboo-trim:make-sim #(9/10 1/10) 'bamboo-trim:reduce-max 100))
(bamboo-trim:run-sim *sim1*)
(defparameter *res1* (bamboo-trim:history *sim1*))
(format t "~&~A~%" *res1*)
(format t "~&~A~%" (bamboo-trim:analysis *sim1*))

(bamboo-trim:sim-tofile *sim1* (merge-pathnames #P"sim1.txt" *data-dir*))

(format t "~&DONE~%")
