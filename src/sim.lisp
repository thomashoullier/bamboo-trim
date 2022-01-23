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
   (history :documentation "Struct holding simulation data for analysis."
            :accessor history :initarg :history)))

(defstruct history
  iters ; vector of iteration indices #(i1 i2 i3 ...)
  heights ; corresponding bamboo heights #(#(h1 h2 h3 ...) (h1 h2 h3 ...))
  choices ; index of bamboo to cut down at the end of iter #(b1 b2 b3 ...)
  )

(defun make-sim (rates algorithm iter-max)
  "Simulation constructor."

  )

(defmethod save-curstate ((sim sim))
  "Append current problem status to history."

  )

(defmethod iterate ((sim sim))
  "Run the next simulation iteration."

  )

(defmethod run-sim ((sim sim))
  "Run the simulation for the planned number of iterations and store
   analysis data."

  )
