(ns test-OrderedLocks
  (:require [clojure.test :refer :all]
            [OrderedLocks :refer :all]
            [salt]
            [salt.lang :refer :all]
            [salt.test-transpiler :as test-transpiler]
            [tlaplus.FiniteSets :refer :all]))

(use-fixtures :each (salt/namespace-fixture 'test-OrderedLocks))

(deftest test-basic
  (is (salt/evaluate
       '{ResourceIds #{101 102}
         TransactionIds #{0 1 2 3}}
       '{transactions {0 {:tid -1}
                       1 {:tid -1}
                       2 {:tid -1}
                       3 {:tid -1}}
         locks {0 #{}
                1 #{}
                2 #{}
                3 #{}}}
       (TypesOk)))

  (is (salt/evaluate
       '{ResourceIds #{101 102}
         TransactionIds #{0 1 2 3}}
       '{transactions {0 {:tid 0
                          :resources #{101 102}
                          :state "created"},
                       1 {:tid 1
                          :resources #{101 102}
                          :state "completed"}
                       2 {:tid -1}
                       3 {:tid -1}}
         locks {0 #{}
                1 #{}
                2 #{}
                3 #{}}}
       (TypesOk)))

  (is (not (salt/evaluate
            '{ResourceIds #{101 102}
              TransactionIds #{0 1 2 3}}
            '{transactions {0 {:tid 0
                               :resources #{101 102}
                               :state "created"},
                            1 {:tid 1
                               :resources #{101 102}
                               :state "completed99"}
                            2 {:tid -1}
                            3 {:tid -1}}
              locks {0 #{}
                     1 #{}
                     2 #{}
                     3 #{}}}
            (TypesOk)))))

(deftest test-Create
  (is (= '#{{transactions' {0 {:tid 0
                               :resources #{101 102}
                               :state "created"}
                            1 {:tid 1
                               :resources #{102}
                               :state "created"}}}
            {transactions' {0 {:tid 0
                               :resources #{101 102}
                               :state "created"}
                            1 {:tid 1
                               :resources #{101}
                               :state "created"}}}
            {transactions' {0 {:tid 0
                               :resources #{101 102}
                               :state "created"}
                            1 {:tid 1
                               :resources #{101 102}
                               :state "created"}}}}
         (salt/simplify "test/OrderedLocks.clj"
                        '{ResourceIds #{101 102}
                          TransactionIds #{0 1}}
                        '{transactions {0 {:tid 0
                                           :resources #{101 102}
                                           :state "created"}
                                        1 {:tid -1}}
                          locks {0 #{}
                                 1 #{}}}
                        (CreateTransaction)
                        :delta))))

(deftest test-Acquire
  (is (= '{locks' {0 #{101}
                   1 #{}}}
         (salt/simplify "test/OrderedLocks.clj"
                        '{ResourceIds #{101 102}
                          TransactionIds #{0 1}}
                        '{transactions {0 {:tid 0
                                           :resources #{101 102}
                                           :state "created"}
                                        1 {:tid -1}}
                          locks {0 #{}
                                 1 #{}}}
                        (AcquireLock)
                        :delta)))

  (is (= '{locks' {0 #{101 102}
                   1 #{}}}
         (salt/simplify "test/OrderedLocks.clj"
                        '{ResourceIds #{101 102}
                          TransactionIds #{0 1}}
                        '{transactions {0 {:tid 0
                                           :resources #{101 102}
                                           :state "created"}
                                        1 {:tid -1}}
                          locks {0 #{101}
                                 1 #{}}}
                        (AcquireLock)
                        :delta)))

  (is (not (salt/simplify "test/OrderedLocks.clj"
                          '{ResourceIds #{101 102}
                            TransactionIds #{0 1}}
                          '{transactions {0 {:tid 0
                                             :resources #{101 102}
                                             :state "created"}
                                          1 {:tid -1}}
                            locks {0 #{101 102}
                                   1 #{}}}
                          (AcquireLock))))

  (is (not (salt/simplify "test/OrderedLocks.clj"
                          '{ResourceIds #{101 102}
                            TransactionIds #{0 1}}
                          '{transactions {0 {:tid 0
                                             :resources #{101 102}
                                             :state "created"}
                                          1 {:tid 1
                                             :resources #{101}
                                             :state "created"}}
                            locks {0 #{}
                                   1 #{101}}}
                          (AcquireLock)))))

(deftest test-Complete
  (is (= '{transactions' {0 {:tid 0
                             :resources #{101 102}
                             :state "completed"}
                          1 {:tid -1}}}
         (salt/simplify "test/OrderedLocks.clj"
                        '{ResourceIds #{101 102}
                          TransactionIds #{0 1}}
                        '{transactions {0 {:tid 0
                                           :resources #{101 102}
                                           :state "created"}
                                        1 {:tid -1}}
                          locks {0 #{101 102}
                                 1 #{}}}
                        (CompleteTransaction)
                        :delta))))

(deftest test-Release
  (is (= '#{{locks' {0 #{102}
                     1 #{}}}
            {locks' {0 #{101}
                     1 #{}}}}
         (salt/simplify "test/OrderedLocks.clj"
                        '{ResourceIds #{101 102}
                          TransactionIds #{0 1}}
                        '{transactions {0 {:tid 0
                                           :resources #{101 102}
                                           :state "completed"}
                                        1 {:tid -1}}
                          locks {0 #{101 102}
                                 1 #{}}}
                        (ReleaseLock)
                        :delta)))

  (is (not
       (salt/simplify "test/OrderedLocks.clj"
                      '{ResourceIds #{101 102}
                        TransactionIds #{0 1}}
                      '{transactions {0 {:tid 0
                                         :resources #{101 102}
                                         :state "created"}
                                      1 {:tid -1}}
                        locks {0 #{101 102}
                               1 #{}}}
                      (ReleaseLock)))))

(deftest test-Invariant
  (is (salt/evaluate '{ResourceIds #{101 102}
                       TransactionIds #{0 1}}
                     '{transactions {0 {:tid 0
                                        :resources #{101 102}
                                        :state "completed"}
                                     1 {:tid -1}}
                       locks {0 #{101 102}
                              1 #{}}}
                     (Invariant)))

  (is (not (salt/evaluate '{ResourceIds #{101 102}
                            TransactionIds #{0 1}}
                          '{transactions {0 {:tid 0
                                             :resources #{101 102}
                                             :state "completed"}
                                          1 {:tid -1}}
                            locks {0 #{101 102}
                                   1 #{101}}}
                          (Invariant)))))

(deftest test-End
  (is (not (salt/evaluate '{ResourceIds #{101 102}
                            TransactionIds #{0 1}}
                          '{transactions {0 {:tid 0
                                             :resources #{101 102}
                                             :state "completed"}
                                          1 {:tid -1}}
                            locks {0 #{101 102}
                                   1 #{}}}
                          (End))))
  (is (not (salt/evaluate '{ResourceIds #{101 102}
                            TransactionIds #{0 1}}
                          '{transactions {0 {:tid 0
                                             :resources #{101 102}
                                             :state "completed"}
                                          1 {:tid 0
                                             :resources #{101 102}
                                             :state "completed"}}
                            locks {0 #{101 102}
                                   1 #{}}}
                          (End))))
  (is (not (salt/evaluate '{ResourceIds #{101 102}
                            TransactionIds #{0 1}}
                          '{transactions {0 {:tid 0
                                             :resources #{101 102}
                                             :state "completed"}
                                          1 {:tid 0
                                             :resources #{101 102}
                                             :state "completed"}}
                            locks {0 #{}
                                   1 #{}}}
                          (End)))))

(deftest test-Spec
  (test-transpiler/check-spec "OrderedLocks"))

;; (test-transpiler/accept "OrderedLocks")
;; (run-tests)
