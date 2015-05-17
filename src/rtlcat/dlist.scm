(load "mk-no-occurs.scm")

(define dlisto
  (lambda (u)
    (conde
      ((== `(end) u))
      ((fresh (ll lx l x r rx rr)
         (== `(node ,l ,x ,r) u)
         (conde
           ((== `(end) l))
           ((== `(node ,ll ,lx ,u) l)))
         (conde
           ((== `(end) r))
           ((== `(node ,u ,rx ,rr) r)))
         )))))

(define tailo
  (lambda (dlist out)
    (conde
      ((== `(end) dlist)
       (== dlist out))
      ((fresh (l x r)
         (== `(node ,l ,x ,r) dlist)
         (conde
           ((== r `(end))
            (== dlist out))
           ((tailo r out)))
         )))))

(define unlinko*
  (lambda (tail acc out)
    (conde
      ((== `(end) tail)
       (== acc out))
      ((fresh (l x r)
         (== `(node ,l ,x ,r) tail)
         (== `(,x . ,acc) out)))
      )))

(define unlinko
  (lambda (dlist out)
    (fresh (tail)
      (tailo dlist tail)
      (unlinko* tail `() out))))


#|

(run 5 (q)
  (fresh (x t)
    (dlisto x)
    (unlinko x q)
    (== `(1 2 3) q)
    ))

|#
