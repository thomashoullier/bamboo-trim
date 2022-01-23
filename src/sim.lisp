;;;; Simulation class: bamboo and cutter managing and monitoring.
(in-package :bamboo-trim)

(defclass sim ()
  ((bamboo :documentation "Bamboo problem instance."
           :reader bamboo :initarg :bamboo)
   (cutter :documentation "Cutter algorithm instance."
           :reader cutter :initarg :cutter)
   (iter-max
    :documentation "Maximum iteration number to run the simulation for."
    :accessor iter-max :initarg :iter-max)
   (history :documentation "Vector holding state history for analysis."
            :accessor history :initarg :history)))

(defstruct state
  iter ; Current iteration index
  heights ; corresponding bamboo heights #(h1 h2 h3 ...)
  choice ; index of bamboo to cut down at the end of iter current iter
  )

(defun make-sim (rates algorithm iter-max)
  "Simulation constructor."
  (let* ((bamboo (make-bamboo rates))
         (cutter (make-cutter algorithm bamboo)))
    (choose cutter) ; Choose the first bamboo to cut down.
    (make-instance 'sim :bamboo bamboo :cutter cutter
                        :iter-max iter-max
                        :history (make-array 0 :fill-pointer 0))))

(defmethod save-curstate ((sim sim))
  "Append current problem status to history."
  (with-slots ((history history) (bamboo bamboo) (cutter cutter)) sim
    (vector-push-extend
     (make-state :iter (iter bamboo)
                 :heights (alexandria:copy-array (heights bamboo))
                 :choice (chosen-bamboo cutter))
     history)))

(defmethod iterate ((sim sim))
  "Run the next simulation iteration."
  (with-slots ((bamboo bamboo)
               (cutter cutter)) sim
    (cut bamboo (chosen-bamboo cutter)) ; Cut the bamboo chosen by cutter.
    (grow bamboo) ; Grow all the bamboos.
    (choose cutter) ; Select the next bamboo to cut.
    ))

(defmethod run-sim ((sim sim))
  "Run the simulation for the planned number of iterations and store
   analysis data."
  (save-curstate sim) ; Save first iteration.
  (with-slots ((bamboo bamboo) (iter-max iter-max)) sim
    (loop while (< (iter bamboo) iter-max) do
      (iterate sim)
      (save-curstate sim))))

(defmethod report-history ((sim sim))
  "Return a copy of the history."
  (alexandria:copy-array (history sim)))
