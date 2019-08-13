(comment "example spec ported from
https://github.com/tlaplus/Examples/blob/master/specifications/transaction_commit/TwoPhase.tla")

(ns TwoPhase
  (:require [salt.lang :refer :all]))

(CONSTANT RM)

(VARIABLE
 rmState
 tmState
 tmPrepared
 msgs)

(defn Message [] (union (maps- [:type #{"Prepared"}
                                :rm RM])
                        (maps- [:type #{"Commit" "Abort"}])))

(defn TPTypeOk []
  (and (contains? (maps- RM #{"working" "prepared" "committed" "aborted"}) rmState)
       (contains? #{"init" "committed" "aborted"} tmState)
       (subset? tmPrepared RM)
       (subset? msgs Message)))

(defn TPInit []
  (REQUIRE- [rmState (fm- [rm RM]
                          "working")
             tmState "init"
             tmPrepared #{}
             msgs #{}]))

(defn TMRcvPrepared [rm]
  (and (= tmState "init")
       (contains? msgs {:type "Prepared"
                        :rm rm})
       (ALLOW- [tmPrepared' (union tmPrepared #{rm})])
       (CHANGED- [tmPrepared])))

(defn TMCommit []
  (and (= tmState "init")
       (= tmPrepared RM)
       (ALLOW- [tmState' "committed"
                msgs' (union msgs #{{:type "Commit"}})])
       (CHANGED- [tmState, msgs])))

(defn TMAbort []
  (and (= tmState "init")
       (ALLOW- [tmState' "aborted"
                msgs' (union msgs #{{:type "Abort"}})])
       (CHANGED- [tmState msgs])))

(defn RMPrepare [rm]
  (and (= (get* rmState rm) "working")
       (ALLOW- [rmState' (EXCEPT rmState [rm] "prepared")
                msgs' (union msgs #{{:type "Prepared"
                                     :rm rm}})])
       (CHANGED- [rmState msgs])))

(defn RMChooseToAbort [rm]
  (and (= (get* rmState rm) "working")
       (ALLOW- [rmState' (EXCEPT rmState [rm] "aborted")])
       (CHANGED- [rmState])))

(defn RMRcvCommitMsg [rm]
  (and (contains? msgs {:type "Commit"})
       (ALLOW- [rmState' (EXCEPT rmState [rm] "committed")])
       (CHANGED- [rmState])))

(defn RMRcvAbortMsg [rm]
  (and (contains? msgs {:type "Abort"})
       (ALLOW- [rmState' (EXCEPT rmState [rm] "aborted")])
       (CHANGED- [rmState])))

(defn TPNext []
  (or (TMCommit)
      (TMAbort)
      (E [rm RM]
         (or (TMRcvPrepared rm)
             (RMPrepare rm)
             (RMChooseToAbort rm)
             (RMRcvCommitMsg rm)
             (RMRcvAbortMsg rm)))))

(defn TPSpec []
  (and (TPInit)
       (always- (TPNext) [rmState tmState tmPrepared msgs])))
