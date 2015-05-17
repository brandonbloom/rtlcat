(load "mk.scm")
(load "preds.scm")



#|

;; forwards
(run 3 (q) (cato '() q))
(run 3 (q) (cato '(1) q))
(run 3 (q) (cato '(1 (2 3) 4) q))
(run 3 (q) (cato '(#t #f) q))
(run 3 (q) (cato '(1 2 swap 3) q))
(run 5 (q) (cato '(5 (dup) call) q))
(run 5 (q) (cato '((1 2) 3 cons) q))

|#
