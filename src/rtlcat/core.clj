(ns rtlcat.core)

(defn literal? [x]
  (number? x))

(defn run [program]
  (when-first [word program]
    (cond
      (literal? word) (cons word (run (next program)))
      (fn? word) (word program)
      :else (throw (Exception. (str "Bad value: " (pr-str word)))))))

(defn split-arg [program]
  (when (seq program)
    (let [[x & xs] program]
      (if (literal? x)
        [x xs]
        (when-let [[y & ys :as ret] (run xs)]
          [y ys])))))

(defn add [program]
  (if-let [[x program] (split-arg program)]
    (if-let [[y program] (split-arg program)]
      (cons (+ x y) (run program))
      (throw (Exception. "Underflow")))
    (throw (Exception. "Underflow"))))

(comment

  (run [1 2 add add 3 4 5])

)
