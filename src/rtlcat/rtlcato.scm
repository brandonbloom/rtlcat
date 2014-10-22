(load "mk.scm")

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

(define cato
  (lambda (program out)
    (conde

      ((== '() program) (== '() out))

      ((fresh (word args)
         (== `(,word . ,args) program)
         (conde

            ;; literals
            ((literalo word)
             (fresh (ret)
               (cato args ret)
               (== `(,word . ,ret) out)))


            ;;; intrinsics

            ;; swap ( a b -- b a )
            ((== 'swap word)
             (fresh (a b k ret)
               (cato args `(,a ,b . ,k))
               (cato k ret)
               (== `(,b ,a . ,ret) out)))

             ;; dup ( a -- a a )
             ((== 'dup word)
              (fresh (a k ret)
                (cato args `(,a . ,k))
                (cato k ret)
                (== `(,a ,a . ,ret) out)))

             ;; drop ( a -- )
             ((== 'drop word)
              (fresh (a k)
                (cato args `(,a . ,k))
                (cato k out)))

             ;; call ( q -- )
             ((== 'call word)
              (fresh (q k qk)
                (cato args `(,q . ,k))
                (concato q k qk)
                (cato qk out)))

             ;; cons ( x lst -- newlst )
             ((== 'cons word)
              (fresh (x lst newlst k ret)
                (cato args `(,x ,lst . ,k))
                (== `(,x . ,lst) newlst)
                (cato k ret)
                (== `(,newlst . ,ret) out)))

             ;; dip ( q x -- x )
             ((== 'dip word)
              (fresh (q x k qk)
                (cato args `(,q ,x . ,k))
                (concato `(,x . ,q) k qk)
                (cato qk out)))

             ;; choose ( b t f -- x )
             ((== 'choose word)
              (fresh (b t f x k ret)
                (cato args `(,b ,t ,f . ,k))
                (conde
                  ((== b #t) (== x t))
                  ((== b #f) (== x f)))
                (cato k ret)
                (== `(,x . ,ret) out)))


             ;;; standard library

             ;; if ( b then else -- x )
             ((== 'if word)
              (cato `(call choose . ,args) out))


             ))))))

#|

;; forwards
(run 3 (q) (cato '() q))
(run 3 (q) (cato '(1) q))
(run 3 (q) (cato '(1 (2 3) 4) q))
(run 3 (q) (cato '(#t #f) q))
(run 3 (q) (cato '(1 swap 2 3) q))
(run 5 (q) (cato '(call (dup) 5) q))
(run 5 (q) (cato '(cons 1 (2 3)) q))
(run 5 (q) (cato '(dip (swap) 1 3 5) q))
(run 5 (q) (cato '(choose #t 1 2) q))
(run 5 (q) (cato '(choose #f 1 2) q))
(run 5 (q) (cato '(if #t (dup 2) (dup 4)) q))

;; BACKWARDS! (etc)
(run 5 (q) (cato q '(1 1)))
(run 15 (q) (fresh (x y)
              (== `(,x . (#t . ,y)) q)
              (cato q '(3))))
(run 10 (q)
     (fresh (x lst)
       (== lst '(1 2 3))
       (concato q lst x)
       (cato x lst)))

;; quines!?
(run 20 (q) (cato q q))

|#
