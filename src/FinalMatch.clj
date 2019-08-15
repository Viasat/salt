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

(defn BadGuyStatusFights []
  (and (not= badGuyStatus "defeated")
       (E [hero livingGoodGuys]
          (and
           (ALLOW- [livingGoodGuys' (difference livingGoodGuys #{hero})])
           (if (= hero HasArtifact)
             (and (ALLOW- [badGuyStatus' "hasArtifact"
                           log' (conj (conj log ["BadGuyStatus defeats: " hero])
                                      ["BadGuyStatus has stone"])])
                  (UNCHANGED [toDestroy]))
             (and
              (ALLOW- [log' (conj log ["BadGuyStatus defeats: " hero])])
              (UNCHANGED [badGuyStatus toDestroy])))))))

(defn BadGuyStatusSnaps []
  (and (= badGuyStatus "hasArtifact")
       (= toDestroy -1)
       (ALLOW- [toDestroy' (div (Cardinality livingGoodGuys) 2)
                log' (conj log ["BadGuyStatus snaps"])])
       (UNCHANGED [badGuyStatus livingGoodGuys])))

(defn BadGuyStatusDestroys []
  (and (> toDestroy 0)
       (E [hero livingGoodGuys]
          (and (ALLOW- [toDestroy' (- toDestroy 1)
                        livingGoodGuys' (difference livingGoodGuys #{hero})
                        log' (conj log ["BadGuyStatus destroys: " hero])])
               (UNCHANGED badGuyStatus)))))

(defn BadGuyStatusRetires []
  (and (= badGuyStatus "hasArtifact")
       (= 0 toDestroy)
       (ALLOW- [badGuyStatus' "retired"
                log' (conj log ["BadGuyStatus retires"])])
       (UNCHANGED [toDestroy livingGoodGuys])))

(defn BadGuyStatusDefeated []
  (and (= badGuyStatus "retired")
       (E [hero1 livingGoodGuys]
          (and (contains? Strong hero1)
               (E [hero2 livingGoodGuys]
                  (and
                   (contains? Flight hero2)
                   (ALLOW- [badGuyStatus' "defeated"
                            log' (conj log ["BadGuyStatus defeated by" hero1 hero2])])
                   (UNCHANGED [toDestroy livingGoodGuys])))))))

(line-)

(defn Init []
  (ALLOW- [badGuyStatus "alive"
           livingGoodGuys GoodGuys
           log []
           toDestroy -1]))

(defn Next []
  (or
   (BadGuyStatusFights)
   (BadGuyStatusSnaps)
   (BadGuyStatusDestroys)
   (BadGuyStatusRetires)
   (BadGuyStatusDefeated)))

(line-)

(defn Spec []
  (and
   (Init)
   (always- (Next) vars)
   (WF vars (BadGuyStatusFights))
   (WF vars (BadGuyStatusSnaps))
   (WF vars (BadGuyStatusDestroys))
   (WF vars (BadGuyStatusRetires))
   (WF vars (BadGuyStatusDefeated))))

(defn End []
  (or (= livingGoodGuys #{})
      (= badGuyStatus "defeated")))

(defn Termination []
  (eventually- (always- (End))))
