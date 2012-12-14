;; Author : Ritchie Cai
;; Date   : 2012-12-04

(set-macro-character #\[ #'(lambda (stream macro-char)
                             (declare (ignore macro-char))
                             (destructuring-bind (h-table h-key)
                                 (read-delimited-list #\] stream t)
                               `(gethash ,h-key ,h-table))))
(set-macro-character #\] (get-macro-character #\) ))

;; (defun new-hash-table (&rest pairs)
;;   (let ((h (make-hash-table :test 'equal)))
;;     (loop for (key value) on pairs by #'cddr do (setf (gethash key h) value))
;;     h))

(set-macro-character #\{ #'(lambda (stream macro-char)
                             (declare (ignore macro-char))
                             (let ((pairs (read-delimited-list #\} stream t)))
                               `(let ((h (make-hash-table :test 'equal)))
                                  (loop :for (key value) :on (list ,@pairs) :by #'cddr
                                     :do (setf (gethash key h) value))
                                  h))))
(set-macro-character #\} (get-macro-character #\) ))

(defun hash-table-print (table)
  (loop
     :initially (format t "{")
     :for key :being the :hash-key of table :using (hash-value value)
     :do (format t "~a:~a, " key value)
     :finally (format t "}")))

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
  (maphash #'(lambda (key value)
               (format t "~a : " key)
               (format t "[zombies   : ~a]~%" [value :zombies])
               (loop
                  :initially (format t "    [neighbors : ")
                  :for k :being the :hash-key of [value :neighbors] :using (hash-value v)
                  :do (format t "~a:~a, " k v)
                  :finally (format t "]~%"))
               (format t "    [exptected : ~a]~%~%" (float [value :expected])))
           table))

(defun expectations (node)
  (loop
     :for key :being the :hash-key of [node :neighbors] :using (hash-value value)
     :if (< value 1) :collect value :into possible
     :else :if (= value 1) :sum 1 :into absolute
     :finally (setf [node :expected] (+ absolute (mean (probabilities possible))))))

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


(defun read-data (input-stream)
  (destructuring-bind (nodes edges steps)
          (read-numbers-from-string (read-line input-stream nil))
        (let ((table (make-hash-table :test #'equal :size nodes)))
          (format t "~%nodes=~a~%edges=~a~%steps=~a~%" nodes edges steps)
          (dotimes (i nodes) (setf [table i] {:zombies 0 :neighbors {} :expected 0}))

          ;; get edges
          (do ((counter 1 (incf counter)))
              ((> counter edges))
            (let ((pair (read-numbers-from-string (read-line input-stream nil))))
              (setf [[[table (first pair)]  :neighbors] (second pair)] 0)
              (setf [[[table (second pair)] :neighbors] (first pair)]  0)))

          ;; get nodes
          (do ((counter 0 (incf counter)))
              ((>= counter nodes))
            (setf [[table counter] :zombies] (read-from-string (read-line input-stream nil))))

          ;; update table
          (maphash #'(lambda (key value)
                       (loop :for i :being the :hash-key of [value :neighbors]
                          :do (setf [[[table i] :neighbors] key]
                                    (/ 1 (hash-table-count [value :neighbors])))))
                   table)
          ;; calculate exptected zombies for next round
          (maphash #'(lambda (key value)
                       (declare (ignore key))
                       (expectations value))
                   table)
          (format t "~%Table~%")
          (print-table    table)
          table)))

(defun main (filename)
  (with-open-file (in filename :direction :input)
    ;; ignore the first line for now
    (do ((test-count (read-from-string (read-line in nil))
                     (decf test-count)))
        ((= test-count 0))
      (declare (integer test-count))
      (print-table (read-data in))
      )))
