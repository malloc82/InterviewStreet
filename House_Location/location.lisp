;; Authoer : Ritchie Cai
;; Date    : 2012-01-10

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


(defstruct (location
             (:constructor make-location (xy &key (x (float (first xy))) (y (float (second xy))))))
  (x 0.0 :type float)
  (y 0.0 :type float))

(defun distance (a b)
  (declare (location a b))
  (sqrt (+ (expt (- (location-x a) (location-x b)) 2)
           (expt (- (location-y a) (location-y b)) 2))))

(defmacro eval-formula1 (expr)
  (cond
    ((atom expr)
     expr)
    ((or (equal (first expr) 'expt)
         (equal (first expr) 'sqrt))
     expr)
    ((and (listp expr) (= (length expr) 1))
     `(eval-formula ,(first expr)))
    (t `(,(second expr) ,(first expr) (eval-formula ,(cddr expr))))))

(defmacro eval-formula (expr)
  (let ((operands   '())
        (operators  '())
        (precedence '((* /) (+ -))))
    (cond
      ((atom expr) expr)
      ((equal (first expr) 'sqrt)
       `(sqrt (eval-formula ,(second expr))))
      ((equal (first expr) 'expt)
       `(expt (eval-formula ,(second expr)) (eval-formula ,(third expr))))
      ((= (length expr) 1)
       `(eval-formula ,(first expr)))
      (t (loop
            :for (token op) :on expr :by #'cddr
            :if (null op)
            :do (return
                  ;; can do one call with more than two arguments if possible
                  (loop
                     :initially (push `(eval-formula ,token) operands)
                     :while operators
                     :do (push (loop :with result = `(,(pop operators) ,(pop operands) ,(pop operands))
                                  :while (equal (first operators) (first result))
                                  :do (progn
                                        (pop operators)
                                        (nconc result (list (pop operands))))
                                  :finally (return result))
                               operands)
                     :finally (return (first operands)))

                  ;; ;; Work but will not take advantage that op functions can take
                  ;; ;; more than two arguments
                  ;; (loop
                  ;;    :initially (push token operands)
                  ;;    :for op :in operators
                  ;;    :do (push `(,(pop operators) ,(pop operands) ,(pop operands)) operands)
                  ;;    :finally (return (first operands)))
                  )
            :do (progn
                  (push `(eval-formula ,token) operands)
                  (if (null operators)
                      (push op operators)
                      (loop :for group :in precedence
                          :do (let ((incoming (member op group))
                                    (top      (member (first operators) group)))
                                (cond
                                  (incoming
                                   (push op operators)
                                   (return))
                                  ((and (not incoming) top)
                                   (push (list (pop operators)
                                               `(eval-formula ,(pop operands))
                                               `(eval-formula ,(pop operands))) operands)
                                   (push op operators)
                                   (return))))))))))))


(defun calculate-location (a b kimberly bob jack janet)
  (let* ((1/a^2 (/ 1 (expt a 2)))
         (1/b^2 (/ 1 (expt b 2)))
         (Pax (+ (- 1/a^2 1) (- 1/b^2 1)))
         (Pbx (* 2 (+ (- (location-x bob) (* (location-x kimberly) 1/a^2))
                      (- (location-x janet) (* (location-x jack) 1/b^2)))))
         (Pcx (+ (- (* (expt (location-x kimberly) 2) 1/a^2) (expt (location-x bob) 2))
                 (- (* (expt (location-x jack) 2) 1/b^2) (expt (location-x janet) 2))))
         (Pay (+ (- 1/a^2 1) (- 1/b^2 1)))
         (Pby (* 2 (+ (- (location-y bob)   (* (location-y kimberly) 1/a^2))
                      (- (location-y janet) (* (location-y jack)     1/b^2)))))
         (Pcy (+ (- (* (expt (location-y kimberly) 2) 1/a^2) (expt (location-y bob) 2))
                 (- (* (expt (location-y jack) 2) 1/b^2) (expt (location-y janet) 2))))
         (Kx (- (/ (expt Pbx 2) (* 4 Pax)) Pcx))
         (Ky (- (/ (expt Pby 2) (* 4 Pay)) Pcy)))
    (format *standard-output* "X : a=~a, b=~a, c=~a~%" Pax Pbx Pcx)
    (format *standard-output* "Y : a=~a, b=~a, c=~a~%" Pay Pby Pcy)
    (format *standard-output* "Kx = ~a~%" Kx)
    (format *standard-output* "Ky = ~a~%" Ky)
    (format *standard-output* "y = ~a~%"  (/ (- Pby) (* 2 Pay)))
    (format *standard-output* "x1 = ~a, x2 = ~a~%"
            (/ (- Pbx (- (sqrt (- (expt Pbx 2) (* 4 Pax (+ Pcx Ky)))))) (* 2 Pax))
            (/ (- Pbx (+ (sqrt (- (expt Pbx 2) (* 4 Pax (+ Pcx Ky)))))) (* 2 Pax)))
    (list Pax Pbx Pcx Pay Pby Pcy)))

(defun read-input (input)
  (destructuring-bind (a b)
      (read-numbers-from-string (read-line input nil))
    (let* ((kimberly (make-location (read-numbers-from-string (read-line input nil))))
           (bob      (make-location (read-numbers-from-string (read-line input nil))))
           (jack     (make-location (read-numbers-from-string (read-line input nil))))
           (janet    (make-location (read-numbers-from-string (read-line input nil)))))
      (format *standard-output* "kimberly : ~a, ~a~%" (location-x kimberly) (location-y kimberly))
      (format *standard-output* "bob      : ~a, ~a~%" (location-x bob) (location-y bob))
      (format *standard-output* "jack     : ~a, ~a~%" (location-x jack) (location-y jack))
      (format *standard-output* "janet    : ~a, ~a~%" (location-x janet) (location-y janet))
      (list a b kimberly bob jack janet))))

(defun test (filename)
  (with-open-file (input filename :direction :input)
    (apply #'calculate-location (read-input input))))
