;; Author : Ritchie Cai
;; Date   : 2012-12-04

(defun read-numbers-from-string (line)
  (when (> (length line) 0)
    (do ((numbers '()) (index 0))
        (())
      (multiple-value-bind (number next)
          (read-from-string line nil nil :start index)
        (if number
            (progn
              (push number numbers)              
              (setq index next))
            (return (nreverse numbers)))))))

(defun print-table (table)
  (loop :for key :being the :hash-key of table :using (hash-value value)
     :do (progn
           (format t "~a  :  " key)
           (format t "zombies : ~a " (getf value :zombies))
           (format t "neighbors : ~a~%" (getf value :neighbors))
           (format t "      channels : ~%")
           (loop :for k :being the :hash-key of (getf value :channel) :using (hash-value v)
              :do (format t "            [~a : ~a] ~%" k v)))))

(defun setup-channels (table)
  (loop
     :for key :being the :hash-key of table :using (hash-value value)
     :do (symbol-macrolet ((neighbors (getf value :Neighbors))
                           (zombies   (getf value :zombies)))
           (loop
              :with count = (length neighbors)
              :for i from 1 to zombies
              :do (loop :for n :in neighbors
                     :do (symbol-macrolet ((channel (getf (gethash n table) :channel)))
                           (unless channel
                             (setf channel (make-hash-table))
                             (setf (gethash :count channel) 0)) 
                           (push (/ 1 count) (gethash key channel))
                           (incf (gethash :count channel)))))))) 

(defun get-input (filename)
  (with-open-file (in filename :direction :input)
    ;; ignore the first line for now
    (do ((test-count (read-from-string (read-line in nil))
                     (decf test-count)))
        ((= test-count 0))
      (destructuring-bind (nodes roads steps)
          (read-numbers-from-string (read-line in nil))
        (let ((junctions (make-hash-table :test #'equal :size nodes)))
          (format t "~%nodes=~a~%roads=~a~%steps=~a~%" nodes roads steps)
          ;; (print "edges:")
          (do ((counter 1 (incf counter)))
              ((> counter roads))
            (let ((pair (read-numbers-from-string (read-line in nil))))
              ;;(print pair)
              (push (second pair) (getf (gethash (first pair)  junctions)
                                        :Neighbors))
              (push (first pair)  (getf (gethash (second pair) junctions)
                                        :Neighbors))))

          ;; (princ "vertices:")
          (do ((counter 0 (incf counter)))
              ((>= counter nodes))
            (setf (getf (gethash counter junctions) :zombies)
                  (read-from-string (read-line in nil))))

          (format t "~%Table~%")
          (setup-channels junctions)
          (print-table    junctions))))))
