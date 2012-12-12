;; Author : Ritchie Cai
;; Date   : 2012-12-04

(set-macro-character #\[ #'(lambda (stream macro-char)
                             (declare (ignore macro-char))
                             (destructuring-bind (h-table h-key)
                                 (read-delimited-list #\] stream t)
                               `(gethash ,h-key ,h-table))))
(set-macro-character #\] (get-macro-character #\) ))

(set-macro-character #\{ #'(lambda (stream macro-char)
                             (declare (ignore macro-char))
                             (let ((pairs (read-delimited-list #\} stream t))
                                   (h (make-hash-table :test 'equal)))
                               (loop :for (key value) on pairs by #'cddr
                                  :do (setf (gethash key h) value))
                               h)))
(set-macro-character #\} (get-macro-character #\) ))

;; (defun {} (&rest pairs)
;;   (let ((h (make-hash-table :test 'equal)))
;;     (loop for (key value) on pairs by #'cddr do (setf (gethash key h) value))
;;     h))

;; (defmacro [] (h-table key)
;;   (let ((k key)
;;         (table h-table))
;;     `(gethash ,k ,table)))

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
  (expectations table)
  (loop :for key :being the :hash-key of table :using (hash-value value)
     :do (progn
           (format t "~a  :  " key)
           (format t "zombies : ~a " (getf value :zombies))
           (format t "neighbors : ~a~%" (getf value :neighbors))
           (format t "      absolute : ~a~%" (getf value :absolute))
           (format t "      expected : ~a, ~a~%" (getf value :expected) (float (getf value :expected)))
           (format t "      samples  : ~a~%" (getf value :samples))
           ;; (format t "      channels : ~%")
           ;; (loop :for k :being the :hash-key of (getf value :channel) :using (hash-value v)
           ;;    :do (format t "            [~a : ~a] ~%" k v))
           (format t "~%")
           )))

(defun setup-channels (table)
  (loop
     :for key :being the :hash-key of table :using (hash-value value)
     :do (symbol-macrolet ((neighbors (getf value :Neighbors))
                           (zombies   (getf value :zombies)))
           (loop :with count = (length neighbors)
              :for n :in neighbors
              :do (symbol-macrolet ((channel (getf [table n] :channel)))
                    (unless channel
                      (setf channel {})
                      (setf [channel :count] 0)
                      (setf (getf [table n] :absolute) 0)) 
                    (dotimes (i zombies)
                      (push (/ 1 count) [channel key])
                      (incf [channel :count])
                      (if (= count 1)
                          (incf (getf [table n] :absolute))
                          (push (/ 1 count) (getf [table n] :samples)))))))))

(defun expectations (table)
  (loop :for key :being the :hash-key of table
     :do (setf (getf [table key] :expected)
               (+ (mean (probabilities (getf [table key] :samples)))
                  (getf [table key] :absolute)))))

(defun mean (samples_list)
  (loop :for sample :in samples_list sum (* (car sample) (cdr sample))))

(defun probabilities (sample_list)
  (labels ((P (samples positive)
             (cond
               ((equal 0 positive)
                (reduce #'*
                        (mapcar #'(lambda (x) (- 1 x))
                                samples)))
               ((null (cdr samples)) (if (= positive 1) (first samples) 0))
               (t (+ (* (first samples) (P (cdr samples) (- positive 1)))
                     (* (- 1 (first samples)) (P (cdr samples) positive)))))))
    (loop :for i from 0 to (length sample_list) collect `(,i . ,(P sample_list i)))))


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
              (push (second pair) (getf [junctions (first pair)]  :Neighbors))
              (push (first pair)  (getf [junctions (second pair)] :Neighbors))))

          ;; (princ "vertices:")
          (do ((counter 0 (incf counter)))
              ((>= counter nodes))
            (setf (getf [junctions counter] :zombies)
                  (read-from-string (read-line in nil))))

          (format t "~%Table~%")
          (setup-channels junctions)
          (print-table    junctions)
          (print (loop :for key being the hash-key of junctions using (hash-value value)
                    :sum (+ (getf value :expected) (getf value :absolute)))))))))
