(ns clj101.redux)

(defn r-apply [s [f & args]]
  (apply f s args))

(defn redux [state actions]
  (transduce (map identity) r-apply state actions))


(def rd
 (let [v (volatile! 0)]
   (fn
     ([]
      @v)
     ([_]
      (let [x @v]
        (vreset! v 0)
        x))
     ([_ e]
      (vswap! v + e)))))

(defn xf [rf]
  (fn
    ([] (rf))
    ([s] (rf s))
    ([s e] (if (odd? e) (rf s e) s))))

(transduce (comp (filter odd?) (map inc)) + 0 (range 100000))
