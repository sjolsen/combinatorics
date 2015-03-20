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
