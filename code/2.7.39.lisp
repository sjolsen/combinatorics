(defun f (n a b δ)
  "Computes the number of subsets of [a, b] with n elements having at least δ
spaces between each."
  (cond ((< (- b a) (* (1- n) (1+ δ)))
         nil)
        ((= n 1)
         (loop
            for x from a to b
            collecting (list x)))
        (t
         (loop
            for x from a to b
            nconcing (mapcar (lambda (xs) (list* x xs))
                             (f (1- n) (+ x δ 1) b δ))))))
