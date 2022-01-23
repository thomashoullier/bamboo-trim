;;;; Using bamboo-trim
(asdf:load-system "bamboo-trim")

;;; Minimal example: two bamboos with reduce-max for 100 iterations.
(defparameter *sim1*
  (bamboo-trim:make-sim #(1/10 9/10) 'bamboo-trim:reduce-max 100))
(bamboo-trim:run-sim *sim1*)
(defparameter *res1* (bamboo-trim:history *sim1*))
(format t "~&~A~%" *res1*)
