(ns Tic
  (:require [salt.lang :refer :all]))

(VARIABLE turn board)

(def vars VARS-)

(defn TypesOk []
  (and (contains? #{"x" "o"} turn)
       (A [x #{1 2 3}]
          (A [y #{1 2 3}]
             (contains? #{"x" "o" "_"} (get* (get* board x) y))))))

(defn Init []
  (and (= turn "x")
       (= board [["_" "_" "_"]
                 ["_" "_" "_"]
                 ["_" "_" "_"]])))

(defn WinRow [x]
  (let [p (get* (get* board x) 1)]
    (and (not= p "_")
         (= p (get* (get* board x) 2))
         (= p (get* (get* board x) 3)))))

(defn WinCol [y]
  (let [p (get* (get* board 1) y)]
    (and (not= p "_")
         (= p (get* (get* board 2) y))
         (= p (get* (get* board 3) y)))))

(defn WinDiag []
  (or (let [p (get* (get* board 1) 1)]
        (and (not= p "_")
             (= p (get* (get* board 2) 2))
             (= p (get* (get* board 3) 3))))
      (let [p (get* (get* board 1) 3)]
        (and (not= p "_")
             (= p (get* (get* board 2) 2))
             (= p (get* (get* board 3) 1))))))

(defn Winner []
  (or (E [x #{1 2 3}]
         (WinRow x))
      (E [y #{1 2 3}]
         (WinCol y))
      (WinDiag)))

(defn OtherPlayer [player]
  (if (= player "x")
    "o"
    "x"))

(defn Move [player]
  (and (not (Winner))
       (= turn player)
       (E [x #{1 2 3}]
          (E [y #{1 2 3}]
             (and
              (= (get* (get* board x) y) "_")
              (= board' (EXCEPT board [x] (EXCEPT (get* board x) [y] player)))
              (= turn' (OtherPlayer player)))))))

(defn Next []
  (or (Move "x")
      (Move "o")))

(line-)

(defn Spec []
  (and (Init)
       (always- (Next) vars)
       (WF vars (Next))))

(defn Termination []
  (eventually- (always- (Winner))))
