(load "mk.scm")
(load "preds.scm")
(load "dlist.scm")

(define rewriteo
  (lambda (program pattern replacement out)
    (fresh (prefix suffix)
      (subseqo program prefix pattern suffix)
      (subseqo out prefix replacement suffix))))

(define cato
  (lambda (program out)
    (conde
      ((fresh (x y)
         (rewriteo program `(,x ,y swap) `(,y ,x) out)))
      ;...
      )))


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
