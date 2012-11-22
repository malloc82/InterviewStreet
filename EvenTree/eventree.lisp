;; Auther : Ritchie Cai
;; Date   : 2012-11-19

(defun parse-line (line)
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

(defun collect-input (&key (filename nil))
  (if filename
      (with-open-file (input-stream filename :direction :input)
        (do ((input-data '())
             (line (read-line input-stream nil)
                   (read-line input-stream nil)))
            ((null line) (nreverse input-data))
          (push (parse-line line) input-data)))
      (do ((input-data '())
             (line (read-line *standard-input* nil)
                   (read-line *standard-input* nil)))
            ((null line) (nreverse input-data))
          (push (parse-line line) input-data))))

(defun make-node (number) (list `(,number . 1))) 
(defun make-tree (pair)   (list `(,(second pair) . 2) (make-node (first pair))))

(defmacro node-value (tree)    `(caar ,tree))
(defmacro node-count (tree)    `(cdar ,tree))
(defmacro node-children (tree) `(cdr ,tree))

(defun push-tree (small-tree main-tree)
  (if (= (node-value small-tree) (node-value main-tree))
      (let ((increment (- (node-count small-tree) 1)))
        (setf (node-children main-tree)
              (append (node-children main-tree) (node-children small-tree)))
        (incf (node-count main-tree) increment)
        increment)
      (loop
         :for child :in (node-children main-tree)
         :do (let ((increment (push-tree small-tree child)))
               (when (> increment 0)
                 (incf (node-count main-tree) increment)
                 (return increment)))
         :finally (return 0))))

(defun populate-tree ()
  (let ((input-data (collect-input))
        (*forest* nil))
    (pop input-data)
    (setq *forest* (list (make-tree (pop input-data))))
    (do ((tree (if input-data (make-tree (pop input-data)) nil)
               (if input-data (make-tree (pop input-data)) nil)))
        ((null tree))
      (when (= (push-tree tree (first *forest*)) 0)
        (nconc *forest* (list tree))))
    *forest*))

(defun count-even-trees (tree)
  (loop
     :with count = 0
     :for child :in (node-children tree)
     :do (progn
           (when (evenp (node-count child)) (incf count))
           (incf count (count-even-trees child)))
     :finally (return count)))

(print (count-even-trees (first (populate-tree))))
