(ns salt.test-TwoPhase
  (:require [clojure.test :refer :all]
            [salt]
            [salt.lang :refer :all]
            [tlaplus.Sequences :refer :all]
            [TwoPhase :refer :all]))

(use-fixtures :each (salt/namespace-fixture 'salt.test-TwoPhase))

(deftest test-basic
  (is (= #{{msgs' #{{:type "Prepared"
                     :rm :rm2}}
            rmState' {:rm1 "working"
                      :rm2 "prepared"}}

           {rmState' {:rm1 "working"
                      :rm2 "aborted"}}

           {msgs' #{{:type "Abort"}}
            tmState' "aborted"}

           {rmState' {:rm1 "aborted"
                      :rm2 "working"}}})
      (salt/simplify "test/TwoPhase.clj"
                     {RM #{:rm1 :rm2}}
                     {rmState {:rm1 "working"
                               :rm2 "working"}
                      tmState "init"
                      tmPrepared #{}
                      msgs #{}}
                     (TPNext)
                     :delta))

  (is (= '#{{tmPrepared' #{:rm1}}

            {msgs' #{{:type "Abort"}
                     {:type "Prepared"
                      :rm :rm1}}
             tmState' "aborted"}

            {rmState' {:rm1 "prepared"
                       :rm2 "aborted"}}

            {msgs' #{{:type "Prepared"
                      :rm :rm1}
                     {:type "Prepared"
                      :rm :rm2}}
             rmState' {:rm1 "prepared"
                       :rm2 "prepared"}}}
         (salt/simplify "test/TwoPhase.clj"
                        {RM #{:rm1 :rm2}}
                        {rmState {:rm1 "prepared"
                                  :rm2 "working"}
                         tmState "init"
                         tmPrepared #{}
                         msgs #{{:type "Prepared"
                                 :rm :rm1}}}
                        (TPNext)
                        :delta)))

  (is (= #{{rmState {:rm1 "working"
                     :rm2 "prepared"}
            msgs #{{:type "Prepared"
                    :rm :rm2}}}

           {rmState {:rm1 "prepared"
                     :rm2 "working"}
            msgs #{{:type "Prepared"
                    :rm :rm1}}}})
      (salt/simplify "test/TwoPhase.clj"
                     {RM #{:rm1 :rm2}}
                     {msgs' #{{:type "Prepared"
                               :rm :rm1}
                              {:type "Prepared"
                               :rm :rm2}}
                      rmState' {:rm1 "prepared"
                                :rm2 "prepared"}
                      tmState' "init"
                      tmPrepared' #{}}
                     (TPNext)
                     :delta))

  (is (= '#{{msgs' #{{:type "Prepared"
                      :rm :rm1}
                     {:type "Prepared"
                      :rm :rm2}}
             rmState {:rm1 "working"
                      :rm2 "prepared"}}

            {tmPrepared' #{:rm2}}

            {rmState {:rm1 "prepared"
                      :rm2 "working"}}}
         (salt/simplify "test/TwoPhase.clj"
                        {RM #{:rm1 :rm2}}
                        {msgs #{{:type "Prepared", :rm :rm2}}
                         rmState' {:rm1 "prepared", :rm2 "prepared"}
                         tmState' "init"
                         tmPrepared #{}}
                        (TPNext)
                        :delta))))

;; (run-tests)
