;;;; Outputting simulation analysis data to file.
(in-package :bamboo-trim)

(defun state-to-string (state)
  "Return a string from a state entry:
   iter choice h1 h2 ... hn"
  (concatenate 'string (write-to-string (state-iter state)) " "
               (write-to-string (state-choice state)) " "
               (reduce (lambda (x y) (concatenate 'string x " " y))
                       (map 'vector #'write-to-string
                            (map 'vector (lambda (x) (coerce x 'float))
                                 (state-heights state))))))

(defmethod sim-tostring ((sim sim))
  "Output simulation results to a text string. Gnuplot compatible."
  (apply #'concatenate 'string
         "# iter choice h1 h2 ... hn" (format nil "~C" #\newline)
         (loop for state across (history sim)
               collect
               (concatenate 'string (state-to-string state)
                            (format nil "~C" #\newline)))))

(defmethod sim-tofile ((sim sim) filename)
  "Output simulation results to a text file."
  (with-open-file (str filename :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
    (format str "# Bamboo rates~%~{~A~^ ~}~%~%~%"
            (map 'list (lambda (x) (write-to-string (coerce x 'float)))
                 (rates (bamboo sim))))
    (format str "# Maximum bamboo height~%~A~%~%~%"
            (write-to-string (coerce (analysis-max-height (analysis sim))
                                     'float)))
    (format str (sim-tostring sim))))
