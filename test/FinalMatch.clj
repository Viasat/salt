(ns FinalMatch
  (:require [salt.lang :refer :all]
            [tlaplus.Integers :refer :all]
            [tlaplus.Sequences :refer :all]
            [tlaplus.FiniteSets :refer :all]))

(CONSTANT GoodGuys Strong Flight HasArtifact)

(ASSUME (and
         (contains? GoodGuys HasArtifact)
         (subset? Strong GoodGuys)
         (subset? Flight GoodGuys)))

(line-)

(VARIABLE
 badGuyStatus
 toDestroy
 livingGoodGuys
 log)

(def vars VARS-)

(defn TypesOk []
  (and (contains? #{"alive" "hasArtifact" "retired" "defeated"} badGuyStatus)
       (subset? livingGoodGuys GoodGuys)
       (contains? (union (Nat) #{-1}) toDestroy)))

(line-)

(defn BadGuyFights []
  (and (not= badGuyStatus "defeated")
       (E [hero livingGoodGuys]
          (and
           (= livingGoodGuys' (difference livingGoodGuys #{hero}))
           (if (= hero HasArtifact)
             (and (= badGuyStatus' "hasArtifact")
                  (= log' (conj (conj log ["BadGuy defeats: " hero])
                                ["BadGuy has stone"]))
                  (UNCHANGED [toDestroy]))
             (and
              (= log' (conj log ["BadGuy defeats: " hero]))
              (UNCHANGED [badGuyStatus toDestroy])))))))

(defn BadGuySnaps []
  (and (= badGuyStatus "hasArtifact")
       (= toDestroy -1)
       (= toDestroy' (div (Cardinality livingGoodGuys) 2))
       (= log' (conj log ["BadGuy snaps"]))
       (UNCHANGED [badGuyStatus livingGoodGuys])))

(defn BadGuyDestroys []
  (and (> toDestroy 0)
       (E [hero livingGoodGuys]
          (and (= toDestroy' (- toDestroy 1))
               (= livingGoodGuys' (difference livingGoodGuys #{hero}))
               (= log' (conj log ["BadGuy destroys: " hero]))
               (UNCHANGED [badGuyStatus])))))

(defn BadGuyRetires []
  (and (= badGuyStatus "hasArtifact")
       (= 0 toDestroy)
       (= badGuyStatus' "retired")
       (= log' (conj log ["BadGuy retires"]))
       (UNCHANGED [toDestroy livingGoodGuys])))

(defn BadGuyDefeated []
  (and (= badGuyStatus "retired")
       (E [hero1 livingGoodGuys]
          (and (contains? Strong hero1)
               (E [hero2 livingGoodGuys]
                  (and
                   (contains? Flight hero2)
                   (= badGuyStatus' "defeated")
                   (= log' (conj log ["BadGuy defeated by" hero1 hero2]))
                   (UNCHANGED [toDestroy livingGoodGuys])))))))

(line-)

(defn Init []
  (and (= badGuyStatus "alive")
       (= livingGoodGuys GoodGuys)
       (= log [])
       (= toDestroy -1)))

(defn Next []
  (or
   (BadGuyFights)
   (BadGuySnaps)
   (BadGuyDestroys)
   (BadGuyRetires)
   (BadGuyDefeated)))

(line-)

(defn Spec []
  (and
   (Init)
   (always- (Next) vars)
   (WF vars (BadGuyFights))
   (WF vars (BadGuySnaps))
   (WF vars (BadGuyDestroys))
   (WF vars (BadGuyRetires))
   (WF vars (BadGuyDefeated))))

(defn End []
  (or (= livingGoodGuys #{})
      (= badGuyStatus "defeated")))

(defn Termination []
  (eventually- (always- (End))))
