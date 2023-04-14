;;Suppose an urn contains N total balls of K colors
(defun choose (n k)
  (\
   (fac n)
   (* (fac k) (fac (- n k)))))

(defun fac (n)
  (if (zerop n)
      1
      (* n (fac((- n 1))))))

(let* ((ball-counts (list 10 10 10 10 10))
      (K (length ball-counts))
      (N (apply '+ ball-counts)))
  (print ball-counts)
  (print K)
  (print N)
  (print (fac 5)))
