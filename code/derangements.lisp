(defun factorial (n)
  (declare (type (integer 0) n)
           (optimize (speed 3)
                     (debug 0)))
  (labels ((f (n acc)
             (if (zerop n)
                 acc
                 (f (1- n) (* n acc)))))
    (f n 1)))

(defun binom (r k)
  (declare (type real    r)
           (type integer k)
           (optimize (speed 3)
                     (debug 0)))
  (cond ((< k 0) 0)
        ((= k 0) 1)
        (t (/ (loop
                 with acc = 1
                 for i from r above (- r k)
                 do (setf acc (* acc i))
                 finally (return acc))))))

(defun mulnom (above &rest below)
  (apply #'/ (mapcar #'factorial (list* above below))))

(defun derangements (n)
  (labels ((-1^ (n)
             (if (evenp n)
                 1
                 -1)))
    (case n
      (0 1)
      (1 0)
      (t (+ (* n (derangements (1- n)))
            (-1^ n))))))
