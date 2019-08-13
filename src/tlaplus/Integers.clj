(ns tlaplus.Integers)

(def ^:dynamic *Nat* 3)

(defn Nat []
  (set (range 0 (inc *Nat*))))

(defn div [x y]
  (int (/ x y)))
