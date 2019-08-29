(ns test-Tic
  (:require [clojure.test :refer :all]
            [salt]
            [salt.lang :refer :all]
            [Tic :refer :all]))

(use-fixtures :each (salt/namespace-fixture 'test-Tic))

(deftest test-types
  (is (salt/evaluate {}
                     {turn "x"
                      board [["_" "_" "_"]
                             ["_" "_" "_"]
                             ["_" "_" "_"]]}
                     (TypesOk)))

  (is (salt/evaluate {}
                     {turn "o"
                      board [["x" "_" "_"]
                             ["_" "o" "_"]
                             ["_" "_" "x"]]}
                     (TypesOk)))

  (is (not (salt/evaluate {}
                          {turn "x"
                           board [["x" "_" "q"]
                                  ["_" "o" "_"]
                                  ["_" "_" "x"]]}
                          (TypesOk))))

  (is (not (salt/evaluate {}
                          {turn "other"
                           board [["x" "_" "_"]
                                  ["_" "o" "_"]
                                  ["_" "_" "x"]]}
                          (TypesOk)))))

(deftest test-Init
  (is (salt/evaluate {}
                     {turn "x"
                      board [["_" "_" "_"]
                             ["_" "_" "_"]
                             ["_" "_" "_"]]}
                     (Init))))

(deftest test-Move
  (is (= '#{{board' [["x" "_" "_"] ["_" "_" "_"] ["_" "_" "_"]], turn' "o"}
            {board' [["_" "x" "_"] ["_" "_" "_"] ["_" "_" "_"]], turn' "o"}
            {board' [["_" "_" "x"] ["_" "_" "_"] ["_" "_" "_"]], turn' "o"}

            {board' [["_" "_" "_"] ["x" "_" "_"] ["_" "_" "_"]], turn' "o"}
            {board' [["_" "_" "_"] ["_" "x" "_"] ["_" "_" "_"]], turn' "o"}
            {board' [["_" "_" "_"] ["_" "_" "x"] ["_" "_" "_"]], turn' "o"}
            {board' [["_" "_" "_"] ["_" "_" "_"] ["x" "_" "_"]], turn' "o"}
            {board' [["_" "_" "_"] ["_" "_" "_"] ["_" "x" "_"]], turn' "o"}
            {board' [["_" "_" "_"] ["_" "_" "_"] ["_" "_" "x"]], turn' "o"}}
         (salt/simplify "test/Tic.clj"
                        {}
                        {turn "x"
                         board [["_" "_" "_"]
                                ["_" "_" "_"]
                                ["_" "_" "_"]]}
                        (Next)
                        :delta)))

  (is (= '#{{board' [["_" "o" "_"] ["_" "_" "_"] ["_" "_" "_"]], turn' "x"}
            {board' [["o" "_" "_"] ["_" "_" "_"] ["_" "_" "_"]], turn' "x"}
            {board' [["_" "_" "_"] ["o" "_" "_"] ["_" "_" "_"]], turn' "x"}

            {board' [["_" "_" "_"] ["_" "_" "o"] ["_" "_" "_"]], turn' "x"}
            {board' [["_" "_" "_"] ["_" "_" "_"] ["o" "_" "_"]], turn' "x"}
            {board' [["_" "_" "_"] ["_" "o" "_"] ["_" "_" "_"]], turn' "x"}

            {board' [["_" "_" "_"] ["_" "_" "_"] ["_" "o" "_"]], turn' "x"}
            {board' [["_" "_" "_"] ["_" "_" "_"] ["_" "_" "o"]], turn' "x"}
            {board' [["_" "_" "o"] ["_" "_" "_"] ["_" "_" "_"]], turn' "x"}}
         (salt/simplify "test/Tic.clj"
                        {}
                        {turn "o"
                         board [["_" "_" "_"]
                                ["_" "_" "_"]
                                ["_" "_" "_"]]}
                        (Next)
                        :delta)))

  (is (= '#{{board' [["x" "o" "_"]
                     ["_" "o" "_"]
                     ["_" "x" "_"]]}

            {board' [["x" "_" "_"]
                     ["_" "o" "o"]
                     ["_" "x" "_"]]}

            {board' [["x" "_" "_"]
                     ["_" "o" "_"]
                     ["_" "x" "o"]]}

            {board' [["x" "_" "_"]
                     ["o" "o" "_"]
                     ["_" "x" "_"]]}

            {board' [["x" "_" "o"]
                     ["_" "o" "_"]
                     ["_" "x" "_"]]}

            {board' [["x" "_" "_"]
                     ["_" "o" "_"]
                     ["o" "x" "_"]]}}
         (salt/simplify "test/Tic.clj"
                        {}
                        {turn "o"
                         board [["x" "_" "_"]
                                ["_" "o" "_"]
                                ["_" "x" "_"]]}
                        (Next)
                        :delta
                        [turn]))))

(deftest test-Winner
  (is (not
       (salt/evaluate {}
                      {turn "o"
                       board [["x" "_" "_"]
                              ["_" "o" "_"]
                              ["_" "x" "_"]]}
                      (Winner))))

  (is (salt/evaluate {}
                     {turn "o"
                      board [["x" "x" "x"]
                             ["_" "o" "_"]
                             ["_" "x" "_"]]}
                     (Winner)))

  (is (salt/evaluate {}
                     {turn "o"
                      board [["x" "x" "o"]
                             ["_" "o" "_"]
                             ["o" "x" "_"]]}
                     (Winner))))

;; (run-tests)
