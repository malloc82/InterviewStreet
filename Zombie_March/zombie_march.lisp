;; Author : Ritchie Cai
;; Date   : 2012-12-04
(proclaim '(inline mean))

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
               (format t " zombies   : ~2,,,@a~%" [value :zombies])
               (loop
                  :initially (format t "~6,0T neighbors :~19,0T")
                  :for i :from 0
                  :for k :being the :hash-keys of [value :neighbors] :using (hash-value v)
                  :if (and (/= i 0)
                           (= (mod i 9) 0)) :do (format t "~%~19,0T")
                  :do (format t "~2,,,@a:~4,,,a " k v)
                  :finally (format t "~%"))
               (format t "~6,0T exptected : ~a~2&" (float [value :expected])))
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
  (if (<= (length data) 1)
      data
      (let ((pivot (first data)))
        (append (q-sort (remove-if-not #'(lambda (x) (> x pivot)) data))
                (remove-if-not #'(lambda (x) (= x pivot)) data)
                (q-sort (remove-if-not #'(lambda (x) (< x pivot)) data))))))

(defun calc-expectations (table)
  (maphash #'(lambda (key node)
               (declare (ignore key))
               (setf [node :expected]
                     (loop
                        :for neighbor being the :hash-keys of [node :neighbors]
                        :using (hash-value incoming)
                        :sum (* incoming [[table neighbor] :zombies]))))
           table))

(defun process-data (input-stream)
  (destructuring-bind (nodes edges steps)
      (read-numbers-from-string (read-line input-stream nil))
    (let ((table (make-hash-table :test #'equal :size nodes)))
      ;; (format t "~%nodes=~a~%edges=~a~%steps=~a~%" nodes edges steps)
      (dotimes (i nodes)
        (setf [table i] {:zombies 0 :neighbors {} :expected  0}))

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
                   (loop :for i :being the :hash-keys of [value :neighbors]
                      :do (setf [[[table i] :neighbors] key]
                                (/ 1 (hash-table-count [value :neighbors])))))
               table)

      ;; (print-table table)
      
      ;; run simulation
      (simulate table steps)
      
      ;; print result
      (destructuring-bind (a1 a2 a3 a4 a5)
          (map 'list #'round (subseq (q-sort (loop :for key :being the :hash-keys of table
                                                :using (hash-value node)
                                                :collect [node :expected])) 0 5))
        (format *standard-output* "~a ~a ~a ~a ~a~%" a1 a2 a3 a4 a5)
        (force-output *standard-output*)))))

(defun simulate (table ntimes)
  (dotimes (i ntimes table)
    (let ((diffs '()))
      (calc-expectations table)
      (maphash #'(lambda (node content)
                   (declare (ignore node))
                   (push (abs (- [content :expected] [content :zombies])) diffs)
                   (setf [content :zombies] [content :expected]))
               table)
      (when (< (reduce #'max diffs) 0.1) (return)))))

(defun main ()
  (do ((test-count (read-from-string (read-line *standard-input* nil))
                   (decf test-count)))
      ((= test-count 0))
    (declare (integer test-count))
    (process-data *standard-input*)))

(defun test (&key (filename nil))
  (if filename
      (with-open-file (in filename :direction :input)
        (do ((test-count (read-from-string (read-line in nil))
                         (decf test-count)))
            ((= test-count 0))
          (declare (integer test-count))
          (process-data in)))
      (do ((test-count (read-from-string (read-line *standard-input* nil))
                         (decf test-count)))
            ((= test-count 0))
          (declare (integer test-count))
          (process-data *standard-input*))))
