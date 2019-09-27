(ns OrderedLocks
  (:require [salt.lang :refer :all]
            [tlaplus.Integers :refer :all]
            [tlaplus.FiniteSets :refer :all]))

(CONSTANT
 (comment "set of all resource ids that exist")
 ResourceIds
 (comment "set of all the transaction IDs that can exist")
 TransactionIds)

(VARIABLE
 (comment "map from transaction id to transaction record")
 transactions
 (comment "map from transaction id to a set of resource ids representing the resources locked by the transaction")
 locks)

(def vars VARS-)

(defn NewTransaction [tid r-set]
  {:tid tid
   :resources r-set
   :state "created"})

(defn CreateTransaction []
  (E [tid TransactionIds]
     (and
      (not (= tid (get* (get* transactions tid) :tid)))
      (E [r-set (SUBSET ResourceIds)]
         (and
          (> (Cardinality r-set) 0)
          (= transactions' (EXCEPT transactions [tid] (NewTransaction tid r-set)))
          (CHANGED- [transactions]))))))

(defn AcquireLock []
  (E [tid TransactionIds]
     (let [t (get* transactions tid)]
       (and (= tid (get* t :tid))
            (= "created" (get* t :state))
            (E [rid (get* t :resources)]
               (and (not (E [rid2 (difference (get* t :resources)
                                              (get* locks tid))]
                            (< rid2 rid)))
                    (not (E [tid2 TransactionIds]
                            (contains? (get* locks tid2) rid)))
                    (= locks' (EXCEPT locks [tid] #(union % #{rid})))
                    (CHANGED- [locks])))))))

(defn CompleteTransaction []
  (E [tid TransactionIds]
     (let [t (get* transactions tid)]
       (and (= tid (get* t :tid))
            (= "created" (get* t :state))
            (= (get* t :resources) (get* locks tid))
            (= transactions' (EXCEPT transactions [tid :state] "completed"))
            (CHANGED- [transactions])))))

(defn ReleaseLock []
  (E [tid TransactionIds]
     (let [t (get* transactions tid)]
       (and (= tid (get* t :tid))
            (= "completed" (get* t :state))
            (E [rid (get* locks tid)]
               (and
                (= locks' (EXCEPT locks [tid] #(difference % #{rid})))
                (CHANGED- [locks])))))))

(defn TypesOk []
  (and (= (DOMAIN locks) TransactionIds)
       (= (DOMAIN transactions) TransactionIds)
       (A [tid TransactionIds]
          (and
           (contains? (SUBSET ResourceIds) (get* locks tid))
           (let [t (get* transactions tid)]
             (or (and (= #{:tid} (DOMAIN t))
                      (= -1 (get* t :tid)))
                 (and (= #{:tid :resources :state} (DOMAIN t))
                      (contains? #{"created" "completed"} (:state t))
                      (= tid (get* t :tid)))))))))

(defn Init []
  (and (= transactions (fm- [x TransactionIds] {:tid -1}))
       (= locks (fm- [x TransactionIds] #{}))))

(defn Next []
  (or (CreateTransaction)
      (AcquireLock)
      (CompleteTransaction)
      (ReleaseLock)))

(defn Spec []
  (and (Init)
       (always- (Next) vars)
       (WF vars (CreateTransaction))
       (WF vars (AcquireLock))
       (WF vars (CompleteTransaction))
       (WF vars (ReleaseLock))))

(defn Invariant []
  (A [rid ResourceIds]
     (and
      (or (E [t TransactionIds]
             (and
              (contains? (get* locks t) rid)
              (not (E [t2 TransactionIds]
                      (and (contains? (get* locks t2) rid)
                           (not= t2 t))))))
          (not (E [t TransactionIds]
                  (contains? (get* locks t) rid)))))))

(defn End []
  (A [tid TransactionIds]
     (let [t (get* transactions tid)]
       (and (= tid (get* t :tid))
            (= "completed" (get* t :state))))))

(defn Termination []
  (eventually- (always- (End))))
