;; Author : Ritchie Cai
;; Date   : 2012-12-04

(eval-when (:compile-toplevel :load-toplevel :execute) 
  (set-macro-character #\[ #'(lambda (stream macro-char)
                               (declare (ignore macro-char))
                               (destructuring-bind (h-table h-key)
                                   (read-delimited-list #\] stream t)
                                 `(gethash ,h-key ,h-table))))
  (set-macro-character #\] (get-macro-character #\) ))

  (set-macro-character #\{ #'(lambda (stream macro-char)
                               (declare (ignore macro-char))
                               (let ((pairs (read-delimited-list #\} stream t)))
                                 `(let ((h (make-hash-table :test 'equal)))
                                    (loop :for (key value) :on (list ,@pairs) :by #'cddr
                                       :do (setf (gethash key h) value))
                                    h))))
  (set-macro-character #\} (get-macro-character #\) )))

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

(defun q-sort (data)
  (if (<= (length data) 1)
      data
      (let ((pivot (first data)))
        (append (q-sort (remove-if-not #'(lambda (x) (> x pivot)) data))
                (remove-if-not #'(lambda (x) (= x pivot)) data)
                (q-sort (remove-if-not #'(lambda (x) (< x pivot)) data))))))

(defun calc-expectations (table)
  (maphash #'(lambda (key node)
               (declare (ignore key))
               (loop
                  :with possibilities = '() and absolute = 0
                  :for n :being the :hash-key of [node :neighbors] :using (hash-value likely-hood)
                  :do (multiple-value-bind (whole partial)
                          (truncate [[table n] :zombies])
                        (if (< likely-hood 1)
                            (loop :for i from 1 to whole :do (push likely-hood possibilities))
                            (incf absolute whole))
                        (when (> partial 0)
                          (push (* partial likely-hood) possibilities)))
                  :finally (setf [node :expected] (+ absolute
                                                     (mean (probabilities (q-sort possibilities)))))))
           table))

(defvar *p-lookup-table* (make-hash-table :test #'equal))

(defun mean (samples_list)
  (loop :for sample :in samples_list sum (* (car sample) (cdr sample))))

(defun probabilities (sample_list)
  (labels ((P (samples positive)
             (let ((key (list samples positive)))
               (if [*p-lookup-table* key]
                   [*p-lookup-table* key]
                   (setf [*p-lookup-table* key]
                         (cond
                           ((equal 0 positive)
                            (reduce #'*
                                    (mapcar #'(lambda (x) (- 1 x))
                                            samples)))
                           ((null (cdr samples)) (if (= positive 1) (first samples) 0))
                           (t (+ (* (first samples) (P (cdr samples) (- positive 1)))
                                 (* (- 1 (first samples)) (P (cdr samples) positive))))))))))
    (loop :for i from 0 to (length sample_list) collect `(,i . ,(P sample_list i)))))

(defun process-data (input-stream)
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
          (simulate table steps)
          
          (print (loop for i in (subseq (q-sort (loop :for key :being the :hash-key of table
                                                   :using (hash-value node)
                                                   :collect [node :expected])) 0 5)
                    :collect (round i)))
          table)))

(defun simulate (table ntimes)
  (setq *p-lookup-table* (make-hash-table :test #'equal))  
  (dotimes (i ntimes table)
    (let ((diff 0))
      (calc-expectations table)
      (maphash #'(lambda (key node)
                   (declare (ignore key))
                   (incf diff (abs (- [node :expected] [node :zombies])))
                   (setf [node :zombies] (float [node :expected])))
               table)
      (format t "i = ~a, diff = ~a~%" i diff)
      (when (< diff 0.1)
        (format t "Stopped at ~a, diff = ~a~%" i diff)
        (return table)))))

(defun main (filename)
  (with-open-file (in filename :direction :input)
    ;; ignore the first line for now
    (do ((test-count (read-from-string (read-line in nil))
                     (decf test-count))
         (record '()))
        ((= test-count 0) record)
      (declare (integer test-count))
      
      (push (process-data in) record)
      ;; (print-table (first table))
      ;; (format t "Total : ~a~%" (loop :for value being the hash-value of (first table)
      ;;                               :sum [value :expected]))
      
      ;; (format t "-------------------------------------------------~%")
      ;; (print-table (first table))
      (format t "Total : ~a~%" (loop :for value being the hash-value of (first record)
                                    :sum [value :expected]))
      )))
