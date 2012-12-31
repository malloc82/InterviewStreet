;; Author : Ritchie Cai
;; Date   : 2012-12-04
;; Enter your code here. Read input from STDIN. Print output to STDOUT

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

(defun print-ht (table)
  (loop
     :initially (format t "{")
     :for key :being the :hash-keys of table :using (hash-value value)
     :for i :from 1
     :do (format t "~a:~a, " key value)
     :if (= (mod i 8) 0) :do (format t "~%")
     :finally (format t "}")))

(defun print-table (table)
  (maphash #'(lambda (key value)
               (format t "~2,,,@a~4,0T: " key)
               (format t " zombies   : ~2,,,@a~%" (zombies value))
               (loop
                  :initially (format t "~6,0T neighbors :~19,0T")
                  :for i :from 0
                  :for k :in (neighbors value)
                  :if (and (/= i 0)
                           (= (mod i 9) 0)) :do (format t "~%~19,0T")
                  :do (format t "~2,,,@a:~4,,,a " (first k) (second k))
                  :finally (format t "~%"))
               (format t "~6,0T exptected : ~a~2&" (float (expected value))))
           table))

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

(defun q-sort (data)
  (declare (list data))
  (if (<= (length data) 1)
      data
      (let ((pivot (first data)))
        (declare (ratio pivot))
        (append (q-sort (remove-if-not #'(lambda (x) (> x pivot)) data))
                (remove-if-not #'(lambda (x) (= x pivot)) data)
                (q-sort (remove-if-not #'(lambda (x) (< x pivot)) data))))))

(defmacro neighbors (entry) `(first  ,entry))
(defmacro zombies   (entry) `(second ,entry))
(defmacro expected  (entry) `(third  ,entry))
(defmacro total     (entry) `(third  ,entry))

(defun process-data (input-stream)
  (destructuring-bind (nodes edges steps)
      (read-numbers-from-string (read-line input-stream nil))
    (let ((table (make-hash-table :test #'equal :size nodes)))
      (dotimes (i nodes) (setf [table i] (list () 0 0)))

      ;; get edges
      (do ((counter 1 (incf counter)))
          ((> counter edges))
        (let ((pair (read-numbers-from-string (read-line input-stream nil))))
          (push (list (second pair) 0) (neighbors [table (first  pair)]))
          (incf (total [table (first  pair)]))
          (push (list (first  pair) 0) (neighbors [table (second pair)]))
          (incf (total [table (second pair)]))))

      ;; get nodes
      (do ((counter 0 (incf counter)))
          ((>= counter nodes))
        (setf (zombies [table counter]) (read-from-string (read-line input-stream nil))))

      ;; update table
      (maphash #'(lambda (key value)
                   (declare (ignore key))
                   (loop :for entry :in (neighbors value)
                      :do (setf (second entry) (/ 1 (total [table (first entry)])))))
               table)
      
      ;; run simulation
      (simulate table steps)
      
      ;; print result
      (destructuring-bind (a1 a2 a3 a4 a5)
          (map 'list #'round (subseq (q-sort (loop :for key :being the :hash-keys of table
                                                :using (hash-value node)
                                                :collect (expected node))) 0 5))
        (format *standard-output* "~a ~a ~a ~a ~a~%" a1 a2 a3 a4 a5)
        (force-output *standard-output*)))))

(defun calc-expectations (table)
  (declare (hash-table table))
  (maphash #'(lambda (key node)
               (declare (ignore key) (list node))
               (setf (expected node) (loop :for (n p) :in (neighbors node)
                                        :sum (* (zombies [table n]) p))))
           table))

(defun simulate (table ntimes)
  (declare (integer ntimes) (hash-table table))
  (dotimes (i ntimes table)
    (declare (integer i))
    (let ((diffs '()))
      (declare (list diffs))
      (calc-expectations table)
      (maphash #'(lambda (node content)
                   (declare (ignore node))
                   (push (abs (- (expected content) (zombies content))) diffs)
                   (setf (zombies content) (expected content)))
               table)
      (when (< (reduce #'max diffs) 1/10) (return)))))

(defun main ()
  (do ((test-count (read-from-string (read-line *standard-input* nil))
                   (decf test-count)))
      ((= test-count 0))
    (declare (integer test-count))
    (process-data *standard-input*)))
