(ns rtlcat.core
  (:refer-clojure :exclude [drop]))

(defn quotation? [x]
  (vector? x))

(defn literal? [x]
  (or (number? x)
      (quotation? x)))

(defn word? [x]
  (fn? x))

(defn run [program]
  (when-let [[x & xs] (seq program)]
    (cond
      (literal? x) (cons x (lazy-seq (run xs)))
      (word? x) (x xs)
      :else (throw (Exception. (str "bad value: " (pr-str x)))))))

(defn add [program]
  (when-let [[x & program] (run program)]
    (when-let [[y & program] (run program)]
      (cons (+ x y) program))))

(defn dup [program]
  (when-let [[x & program] (run program)]
    (cons x (cons x program))))

(defn swap [program]
  (when-let [[x & program] (run program)]
    (when-let [[y & program] (run program)]
      (cons y (cons x program)))))

(defn drop [program]
  (next (run program)))

(defn call [program]
  (when-let [[q & program] (run program)]
    (run (concat q program))))

(defn dip [program]
  (when-let [[q & program] (run program)]
    (when-let [[hide & program] (run program)]
      (run (concat [hide] q program)))))

(defn append [program]
  (when-let [[v & program] (run program)]
    (when-let [[x & program] (run program)]
      (cons (conj v x) program))))

(comment

  (run [])
  (run [1])
  (run [1 2 3])
  (run [add 1 2])
  (run [1 2 add add 3 4 5])
  (run [add 1]) ; underflows
  (run [add dup 1])
  (run [swap 5 10])
  (run [add call [dup] 3])
  (run [3 dip [add] 5 7 9])
  (run [5 drop 10 15])
  (run [call append [add 5] 10])

)
