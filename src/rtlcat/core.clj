(ns rtlcat.core)

(defn literal? [x]
  (number? x))

(defn word? [x]
  (fn? x))

(defn run [program]
  (when-let [[x & xs] (seq program)]
    (cond
      (literal? x) (cons x (run xs))
      (word? x) (x xs)
      :else (throw (Exception. (str "bad value: " (pr-str x)))))))

(defn add [program]
  (when-let [[x & program] (run program)]
    (when-let [[y & program] (run program)]
      (lazy-seq
        (cons (+ x y) program)))))

(comment

  (run [])
  (run [1])
  (run [1 2 3])
  (run [add 1 2])
  (run [1 2 add add 3 4 5])

)
