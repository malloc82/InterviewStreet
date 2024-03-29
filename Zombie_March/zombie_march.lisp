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
               (format t " zombies   : ~2,,,@a~%" (node-zombies value))
               (loop
                  :initially (format t "~6,0T neighbors :~19,0T")
                  :for i :from 0
                  ;; :for k :being the :hash-keys of [value :neighbors] :using (hash-value v)
                  :for k :in (node-neighbors value)
                  :if (and (/= i 0)
                           (= (mod i 9) 0)) :do (format t "~%~19,0T")
                  :do (format t "~2,,,@a:~4,,,a " (first k) (second k))
                  :finally (format t "~%"))
               (format t "~6,0T exptected : ~a~2&" (float (node-expected value))))
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

(defstruct node
  (neighbors '() :type list)
  (zombies   0   :type number)
  (expected  0   :type number))

(defmacro node-total (entry) `(node-expected  ,entry))

(defun process-data (input-stream)
  (destructuring-bind (nodes edges steps)
      (read-numbers-from-string (read-line input-stream nil))
    (let ((table (make-hash-table :test #'equal :size nodes)))
      ;; (format t "~%nodes=~a~%edges=~a~%steps=~a~%" nodes edges steps)
      (dotimes (i nodes) (setf [table i] (make-node)))

      ;; get edges
      (do ((counter 1 (incf counter)))
          ((> counter edges))
        (let ((pair (read-numbers-from-string (read-line input-stream nil))))
          (push (list (second pair) 0) (node-neighbors [table (first  pair)]))
          (incf (node-total [table (first  pair)]))
          (push (list (first  pair) 0) (node-neighbors [table (second pair)]))
          (incf (node-total [table (second pair)]))))

      ;; get nodes
      (do ((counter 0 (incf counter)))
          ((>= counter nodes))
        (setf (node-zombies [table counter]) (read-from-string (read-line input-stream nil))))

      ;; update table
      (maphash #'(lambda (key value)
                   (declare (ignore key))
                   (loop :for entry :in (node-neighbors value)
                      :do (setf (second entry) (/ 1 (node-total [table (first entry)])))))
               table)
      
      ;; run simulation
      (simulate table steps)
      
      ;; print result
      (destructuring-bind (a1 a2 a3 a4 a5)
          (map 'list #'round (subseq (q-sort (loop :for key :being the :hash-keys of table
                                                :using (hash-value node)
                                                :collect (node-expected node))) 0 5))
        (format *standard-output* "~a ~a ~a ~a ~a~%" a1 a2 a3 a4 a5)
        (force-output *standard-output*)))))

(defun calc-expectations (table)
  (declare (hash-table table))
  (maphash #'(lambda (key entry)
               (declare (ignore key) (node entry))
               (setf (node-expected entry) (loop :for (n p) :in (node-neighbors entry)
                                        :sum (* (node-zombies [table n]) p))))
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
                   (push (abs (- (node-expected content) (node-zombies content))) diffs)
                   (setf (node-zombies content) (node-expected content)))
               table)
      (when (< (reduce #'max diffs) 1/10) (return)))))

(defun main ()
  (do ((test-count (read-from-string (read-line *standard-input* nil))
                   (decf test-count)))
      ((= test-count 0))
    (declare (integer test-count))
    (process-data *standard-input*)))

(defun test (filename)
  (with-open-file (in filename :direction :input)
    (do ((test-count (read-from-string (read-line in nil))
                     (decf test-count)))
        ((= test-count 0))
      (declare (integer test-count))
      (process-data in))))
