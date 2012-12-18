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
     :for key :being the :hash-key of table :using (hash-value value)
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
                  :for k :being the :hash-key of [value :neighbors] :using (hash-value v)
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

;; (defun calc-expectations (table)
;;   (maphash #'(lambda (key node)
;;                (declare (ignore key))
;;                (loop
;;                   :with possibilities = '() and absolute = 0
;;                   :for n :being the :hash-key of [node :neighbors] :using (hash-value likely-hood)
;;                   :do (multiple-value-bind (whole partial)
;;                           (truncate [[table n] :zombies])
;;                         (if (< likely-hood 1)
;;                             (loop :for i from 1 to whole :do (push likely-hood possibilities))
;;                             (incf absolute whole))
;;                         (when (> partial 0)
;;                           (push (* partial likely-hood) possibilities)))
;;                   :finally (setf [node :expected] (+ absolute
;;                                                      (mean (probabilities  possibilities))))))
;;            table))

(defun calc-expectations (table)
  (maphash #'(lambda (key node)
               (declare (ignore key))
               (loop
                  :with possibilities = '() and absolute = (list :abs 0 :pos '()) and more = t
                  :while more
                  :do (loop
                         :initially (setf more nil)
                         :for neighbor :being the :hash-key of [node :neighbors]
                         :using (hash-value likely-hood)
                         :unless (= (first likely-hood) 0)
                         :do (progn
                               ;; (print likely-hood)
                               (if (= (second likely-hood) 1)
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
                                          (setf more t))))))
                  :finally (progn
                             (setf [node :expected] (+ (getf absolute :abs)
                                                       (mean (probabilities
                                                              (append (getf absolute :pos)
                                                                      possibilities)))))
                             ;; (format t ":key ~a :abs ~a :list ~a :possibilities ~a :expected ~a~%"
                             ;;         key
                             ;;         (getf absolute :abs)
                             ;;         (mean (probabilities
                             ;;                (append (getf absolute :pos) possibilities)))
                             ;;         possibilities
                             ;;         [node :expected])
                             )))
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
                                (list [value :zombies]
                                      (/ 1 (hash-table-count [value :neighbors]))))))
               table)

      ;; run simulation
      (simulate table steps)
      
      ;; print result
      (format t "~a~%" (loop for i in (subseq (q-sort (loop :for key :being the :hash-key of table
                                                         :using (hash-value node)
                                                         :collect [node :expected])) 0 5)
                          :collect (round i)))
      table)))

(defun simulate (table ntimes)
  (setq *p-lookup-table* (make-hash-table :test #'equal))  
  (dotimes (i ntimes table)
    (let ((diffs '()))
      (calc-expectations table)
      (maphash #'(lambda (key node)
                   ;; (declare (ignore key))
                   (push (abs (- [node :expected] [node :zombies])) diffs)
                   (setf [node :zombies] (float [node :expected]))
                   (maphash #'(lambda (neighbor p)
                                (declare (ignore p))
                                (setf (first [[[table neighbor] :neighbors] key])
                                      [node :zombies]))
                            [node :neighbors]))
               table)
      ;; (print-table table)
      (let ((max_diff (apply #'max diffs)))
        ;; (format t "i = ~a, max diff = ~a~%" i max_diff)
        (when (<  max_diff 0.1)
          ;; (format t "Stopped at ~a, diff = ~a~%" i max_diff)
          (return table))))))

(defun main (filename)
  (with-open-file (in filename :direction :input)
    ;; ignore the first line for now
    (do ((test-count (read-from-string (read-line in nil))
                     (decf test-count))
         (record '()))
        ((= test-count 0) record)
      (declare (integer test-count))
      (push (process-data in) record)
      ;; (print-table (first record))
      ;; (format t "Total : ~a~%" (loop :for value being the hash-value of (first record)
      ;;                             :sum [value :expected]))
      
      ;; (format t "-------------------------------------------------~%")
      ;; (print-table (first table))
      ;; (format t "Total : ~a~%" (loop :for value being the hash-value of (first record)
      ;;                               :sum [value :expected]))
      )))
