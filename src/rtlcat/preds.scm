(define concato
  (lambda (l s out)
    (conde
      ((== '() l) (== s out))
      ((fresh (a d res)
         (== `(,a . ,d) l)
         (== `(,a . ,res) out)
         (concato d s res))))))

(define seqo
  (lambda (u)
    (fresh (a d)
      (conde
        ((== '() u) succeed)
        ((== `(,a . ,d) u) succeed)
        (succeed fail)))))

(define quotationo seqo)

(define boolo
  (lambda (u)
    (conde
      ((== u #t) succeed)
      ((== u #f) succeed))))

(define literalo
  (lambda (u)
    (conde
      ((numbero u) succeed)
      ((quotationo u) succeed)
      ((boolo u) succeed))))
