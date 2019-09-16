(ns Clock
  (:require [salt.lang :refer :all]))

(VARIABLE clock)

(defn Init []
  (contains? #{0 1} clock))

(defn Tick []
  (or (and (= clock 0)
           (= clock' 1))
      (and (= clock 1)
           (= clock' 0))))

(defn Spec []
  (and
   (Init)
   (always- (Tick) [clock])))
