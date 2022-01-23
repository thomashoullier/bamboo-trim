;;;; Cutter algorithms.
(in-package :bamboo-trim)

;;; Cutter superclass
(defclass cutter ()
  ((bamboo :documentation "Bamboo problem instance reference."
           :reader bamboo :initarg :bamboo)))

(defmacro make-cutter (algorithm bamboo)
  "Create a solver for bamboo problem instance with given algorithm."
  (make-instance algorithm :bamboo ,bamboo))

(defgeneric choose ()
  (:documentation "Return the index of the bamboo to cut down.
                   nil if none to cut")
  (:method () nil))

;;; Reduce-Max algorithm
(defclass reduce-max (cutter)
  ())

(defmethod find-max-pos ((bamboo bamboo))
  "Return the index of the current tallest bamboo."
  (loop for i from 0
        for h across (heights bamboo)
        with imax = 0 with hmax = 0 do
          (when (> h hmax) (setf hmax h imax i))
        finally (return imax)))

(defmethod choose ((reduce-max reduce-max))
  "Apply reduce-max to current problem iteration."
  (find-max-pos (bamboo reduce-max)))

;;; Reduce-Fastest(x) algorithm

;;; Deadline-Driven Strategy
