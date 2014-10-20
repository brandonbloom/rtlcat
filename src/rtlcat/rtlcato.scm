(load "mk.scm")

(define appendo
  (lambda (l s out)
    (conde
      ((== '() l) (== s out))
      ((fresh (a d res)
         (== `(,a . ,d) l)
         (== `(,a . ,res) out)
         (appendo d s res))))))

(define cato
  (lambda (program o)
    (conde

      ((== '() program) (== '() o))

      ((fresh (word args)
         (== `(,word . ,args) program)
         (conde

            ;; literal numbers
            ((numbero word)
             (fresh (ret)
               (cato args ret)
               (== `(,word . ,ret) o)))

            ;; swap (a b -- b a)
            ((== 'swap word)
             (fresh (a b k ret)
               (cato args `(,a ,b . ,k))
               (cato k ret)
               (== `(,b ,a . ,ret) o)))

             ;; dup (a -- a a)
             ((== 'dup word)
              (fresh (a k ret)
                (cato args `(,a . ,k))
                (cato k ret)
                (== `(,a ,a . ,ret) o)))

             ;; drop (a -- )
             ((== 'drop word)
              (fresh (a k ret)
                (cato args `(,a . ,k))
                (cato k o)))

             ))))))

#|

(run 5 (q) (cato q '(1 1)))

(run 1 (q) (cato q q))

|#
