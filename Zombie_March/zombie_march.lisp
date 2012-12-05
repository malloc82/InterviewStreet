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

(defun get-input (filename)
  (with-open-file (in filename :direction :input)
    ;; ignore the first line for now
    (do ((test-count (read-from-string (read-line in nil))
                     (decf test-count)))
        ((= test-count 0))
      (destructuring-bind (nodes roads steps)
          (read-numbers-from-string (read-line in nil))
        (format t "~%nodes=~a~%roads=~a~%steps=~a~%" nodes roads steps)
        (print "edges:")
        (do ((counter 1 (incf counter)))
            ((> counter roads))
          (print (read-numbers-from-string (read-line in nil))))
        (print "vertices:")
        (do ((counter 1 (incf counter)))
            ((> counter nodes))
          (print (read-numbers-from-string (read-line in nil))))))))
