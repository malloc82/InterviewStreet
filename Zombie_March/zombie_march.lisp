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
               (format t "~2,,,@a~4,0T: " key)
               (format t " zombies   : ~2,,,@a~%" [value :zombies])
               (loop
                  :initially (format t "~6,0T neighbors :~19,0T")
                  :for i :from 0
                  :for k :being the :hash-keys of [value :neighbors] :using (hash-value v)
                  :if (and (/= i 0)
                           (= (mod i 6) 0)) :do (format t "~%~19,0T")
                  :do (format t "~2,,,@a:~9,,,a " k v)
                  :finally (format t "~%"))
               (format t "~6,0T exptected : ~a~2&" (float [value :expected])))
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
                  :with possibilities = '() and absolute = (list :abs 0 :pos '()) and more = t
                  :while more
                  :do (loop
                         :initially (setf more nil)
                         :for neighbor :being the :hash-keys of [node :neighbors]
                         :using (hash-value likely-hood)
                         :unless (= (first likely-hood) 0)
                         :do  (if (= (second likely-hood) 1)
                                  (multiple-value-bind (whole partial)
                                      (truncate (first likely-hood))
                                    (incf (getf absolute :abs) whole)
                                    (when (/= partial 0) (push partial (getf absolute :pos)))
                                    (setf (first likely-hood ) 0)
                                    (setf more t))
                                  (cond ((>= (first likely-hood) 1)
                                         (push (second likely-hood) possibilities)
                                         (decf (first likely-hood))
                                         (setf more t))
                                        ((> (first likely-hood) 0)
                                         (push (* (first likely-hood) (second likely-hood)) possibilities)
                                         (setf (first likely-hood) 0)
                                         (setf more t)))))
                  :finally (setf [node :expected] (+ (getf absolute :abs)
                                                     (mean (probabilities
                                                            (append (getf absolute :pos)
                                                                    possibilities)))))))
           table))

(defun calc-expectations2 (table)
  (maphash #'(lambda (key node)
               (declare (ignore key))
               (setf [node :expected]
                     (loop
                        :for incoming being the :hash-value of [node :neighbors]
                        :sum (* (first incoming) (second incoming)))))
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
                            (apply #'* (mapcar #'(lambda (x) (- 1 x)) samples)))
                           ((null (cdr samples)) (if (= positive 1) (first samples) 0))
                           (t (+ (* (first samples) (P (cdr samples) (- positive 1)))
                                 (* (- 1 (first samples)) (P (cdr samples) positive))))))))))
    (loop :for i from 0 to (length sample_list) collect `(,i . ,(P sample_list i)))))

(defvar *p-tree* nil)
(defvar *p-tree-nodes* (make-hash-table :test #'equal))
(defvar *factorial-cache* (make-hash-table :test #'equal))

(defun probabilities2 (sample_list)
  (loop
     :with X = 1 and P = 1 and x_i = 1
     ;; :for x_i from 1 to (length sample_list)
     :for p_i in sample_list
     ;; :sum x_i into n
     :do (setq X (* X (factorial x_i)))
     :do (setq P (* P (expt p_i x_i)))
     :finally (progn
                (format *query-io* "X = ~a~%" X)
                (format *query-io* "P = ~a~%" P)
                (return (* (/ (factorial (length sample_list)) 1) P)))))

(defun factorial (n)
  (declare (integer n))
  (if [*factorial-cache* n]
      [*factorial-cache* n]
      (setf [*factorial-cache* n]
            (if (= n 0)
                1
                (* n (factorial (- n 1)))))))

(defun process-data (input-stream)
  (destructuring-bind (nodes edges steps)
      (read-numbers-from-string (read-line input-stream nil))
    (let ((table (make-hash-table :test #'equal :size nodes)))
      ;; (format t "~%nodes=~a~%edges=~a~%steps=~a~%" nodes edges steps)
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
                   (loop :for i :being the :hash-keys of [value :neighbors]
                      :do (setf [[[table i] :neighbors] key]
                                (list [value :zombies]
                                      (/ 1 (hash-table-count [value :neighbors]))))))
               table)

      (print-table table)
      
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
      (calc-expectations2 table)
      (maphash #'(lambda (node content)
                   ;; (declare (ignore node))
                   (push (abs (- [content :expected] [content :zombies])) diffs)
                   (setf [content :zombies] [content :expected])
                   (maphash #'(lambda (neighbor p)
                                (declare (ignore p))
                                (setf (first [[[table neighbor] :neighbors] node])
                                      [content :zombies]))
                            [content :neighbors]))
               table)
      (when (< (apply #'max diffs) 0.1) (return)))))

(defun main (&key (filename nil))
  (setq *p-lookup-table* (make-hash-table :test #'equal))
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
