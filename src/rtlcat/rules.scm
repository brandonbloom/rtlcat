(load "mk.scm")
(load "preds.scm")

;; This one is a (half finished) very basic small step interpreter version.
;; What I actually want to do here is implement a verison that *scans*
;; ahead for opportunities for the word rules to match.

(define stacko
  (lambda (stack queue out)

    (conde

      ((== '() queue) (== out stack))

      ((fresh (word q)
         (== `(,word . ,q) queue)
         (conde

           ((literalo word)
            (fresh (s)
              (== `(,word . ,stack) s)
              (stacko s q out)))

           ((== 'swap word)
            (fresh (x y more s)
              (== `(,x ,y . ,more) stack)
              (== `(,y ,x . ,more) s)
              (stacko s q out)))

           ((== 'dup word)
            (fresh (x more s)
              (== `(,x . ,more) stack)
              (== `(,x ,x . ,more) s)
              (stacko s q out)))

           ((== 'drop word)
            (fresh (x s)
              (== `(,x . ,s) stack)
              (stacko s q out)))

           ((== 'call word)
            (fresh (quot s qq)
              (== `(,quot . ,s) stack)
              (concato quot q qq)
              (stacko s qq out)))

           ((== 'cons word)
            (fresh (x lst more s)
              (== `(,x ,lst . ,more) stack)
              (== `((,x . ,lst) . ,more) s)
              (stacko s q out)))

           ;;....

           )))

      )))

(define cato
  (lambda (program out)
    (stacko '() program out)))

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
