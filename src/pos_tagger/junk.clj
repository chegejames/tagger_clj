(defn str
  ([^Object x]
   (if (nil? x) "" (. x (toString))))
  ([x & ys]
     ((fn [^StringBuilder sb more]
          (if more
            (recur (. sb  (append (first more))) (next more))
            (str sb)))
      (new StringBuilder ^String x) ys)))




(defn str
  "With no args, returns the empty string. With one arg x, returns
  x.toString().  (str nil) returns the empty string. With more than
  one arg, returns the concatenation of the str values of the args."
  {:tag String
   :added "1.0"}
  ([] "")
  ([^Object x]
   (if (nil? x) "" (. x (toString))))
  ([x & ys]
     ((fn [^StringBuilder sb more]
          (if more
            (recur (. sb  (append (str (first more)))) (next more))
            (str sb)))
      (new StringBuilder ^String (str x)) ys)))



(defn score[map keys]
  (loop [acc 0 keys (seq keys)]
    (if-let [[_ v] (find map (first keys))]
      (recur (+ acc v) (next keys))
      acc)))
        
