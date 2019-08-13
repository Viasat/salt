(ns tlaplus.FiniteSets
  (:require [clojure.math.combinatorics :as combo]))

(defn SUBSET [s]
  (set (map set (combo/subsets (vec s)))))

(defn Cardinality [s]
  (count s))
