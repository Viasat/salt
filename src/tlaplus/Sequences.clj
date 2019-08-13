(ns tlaplus.Sequences
  (:require [clojure.algo.generic.functor :refer [fmap]]
            [salt.lang :as lang]
            [tlaplus.Integers :as Integers]))

(comment "HEAD TAIL Append \\o Len")

(defn SelectSeq [f x]
  (->> x
       (filter f)
       vec))

(defn SubSeq [v start end]
  (subvec v (dec start) end))

(defn Seq [s]
  (lang/UNION (fmap (fn [n]
                      (lang/set-of-maps-from-sets (range 1 (inc n)) s))
                    (Integers/Nat))))

(binding [Integers/*Nat* 2]
  (Seq #{"a" "b"}))

#{["b"]
  []
  ["a" "a"]
  ["a" "b"]
  ["b" "b"]
  ["a"]
  ["b" "a"]}

(Seq #{"a" "b"})
#{["b" "a" "b"]
  ["a" "a" "a"]
  ["b" "a" "a"]
  ["a" "b" "b"]
  ["b" "b" "b"]
  ["b"]
  ["a" "a" "b"]
  []
  ["a" "a"]
  ["a" "b" "a"]
  ["a" "b"]
  ["b" "b"]
  ["b" "b" "a"]
  ["a"]
  ["b" "a"]}
