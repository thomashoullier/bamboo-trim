;;;; Bamboo problem class.
(in-package :bamboo-trim)

(defclass bamboo ()
  ((rates :documentation "Vector of bamboo growing rates."
          :reader rates :initarg :rates)
   (heights :documentation "Current bamboo heights."
            :reader heights :initarg :heights)
   (iter :documentation "Iteration number: incremented after growing."
         :reader iter :initarg :iter)))

(defun make-bamboo (rates)
  "Bamboo constructor. Starts at iter = 1, the bamboos have grown once."
  (let* ((n (length rates))
         (heights (alexandria:copy-array rates)))
    (make-instance 'bamboo :rates (alexandria:copy-array rates)
                           :heights heights :iter 1)))

;;; Problem operations
(defmethod grow ((bamboo bamboo))
  "Next iteration: make the bamboos grow."
  (with-slots ((heights heights) (rates rates) (iter iter)) bamboo
    (setf heights (map 'vector (lambda (h r) (+ h r)) heights rates))
    (incf iter)))

(defmethod cut ((bamboo bamboo) i)
  "Cut down the chosen bamboo to zero. Indexed from 0."
  (with-slots ((heights heights)) bamboo
    (setf (aref heights i) 0)))
