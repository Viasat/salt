(ns salt.test-suite
  (:require [clojure.pprint :as pprint]
            [clojure.string :as string]
            [clojure.test :refer :all]
            [salt.lang :refer :all]
            [salt.transpiler :as transpiler]
            [tlaplus.Integers :refer :all]
            [tlaplus.Sequences :refer :all]
            [tlaplus.FiniteSets :refer :all]))

(def ^:dynamic *output-docs* false)

(def ^:dynamic *output-debug* false)

(defn- either-println [s]
  (when (or *output-docs*
            *output-debug*)
    (println s)))

(defn- docs-println [s]
  (when *output-docs*
    (println s)))

(defn- debug-println [s]
  (when *output-debug*
    (println s)))

(defn section [s]
  (docs-println s)
  (docs-println "----"))

(defn table-output [column-headers row-headers objects]
  (str "\n"
       (loop [column-headers column-headers
              result "| "]
         (let [[h & more] column-headers]
           (if h
             (recur more (str result h " | "))
             (str (string/trim result) "\n"))))
       (loop [column-headers column-headers
              result "| "]
         (let [[h & more] column-headers]
           (if h
             (recur more (str result "---" " | "))
             (str (string/trim result) "\n"))))
       (loop [row-headers row-headers
              result-rh ""]
         (let [[rh & more-rh] row-headers]
           (if rh
             (recur more-rh (str result-rh
                                 (loop [column-headers (rest column-headers)
                                        result-2 (str "| " rh " | ")]
                                   (let [[h2 & more-h2] column-headers]
                                     (if h2
                                       (recur more-h2 (str result-2 (get-in objects [rh h2]) " | "))
                                       (str (string/trim result-2) "\n"))))))
             (str (string/trim result-rh) "\n"))))))

(def clojure-code-width 60)

(defn code-table-output [salt-form salt-result tla-code tla-result]
  (let [start-code "`"
        end-code "`"
        format-f #(if (= "" (str %))
                    " "
                    (-> (str start-code % end-code)
                        (string/replace  "\n\n" "`<br>` `<br>`")
                        (string/replace "\n" "`<br>`")
                        (string/replace "\\" "\\\\")
                        (string/replace "/\\\\" "/\\")
                        (string/replace "\\\\/" "\\/")
                        (string/replace "|" "\\|")
                        (string/replace "``" "` `")))]
    (docs-println (table-output ["" "salt" "tla+"]
                                ["code" "result"]
                                {"code" {"salt" (format-f (if (and (map? salt-form)
                                                                   (= (set (keys salt-form)) #{:code
                                                                                               :alternate}))
                                                            (:alternate salt-form)
                                                            (with-out-str (pprint/write salt-form :right-margin clojure-code-width))))
                                         "tla+" (format-f tla-code)}
                                 "result" {"salt" (format-f (if (nil? salt-result)
                                                              ""
                                                              (with-out-str (pprint/write salt-result :right-margin clojure-code-width))))
                                           "tla+" (format-f tla-result)}}))))

(defn output
  ([type s]
   (output type s false))
  ([type s checked?]
   (condp = type
     :desc (either-println s)
     :tla-code (when-not checked? (debug-println s))
     nil)))

(defmacro check
  ;; the 'checked?' parameter indicates whether the tla-results have been manually checked in TLA+
  ([desc salt-form tla-code tla-result checked?]
   `(do
      (is (= ~tla-code (transpiler/transpile-single-form ~salt-form)))
      (output :desc ~desc)
      (output :tla-code (str "" ~tla-code "") ~checked?)
      (code-table-output ~salt-form nil ~tla-code
                         (when (not (= :? ~tla-result))
                           ~tla-result))))
  ([desc salt-form salt-result tla-code tla-result checked?]
   `(binding [*ns* (find-ns 'salt.test-suite)]
      (if (and (vector? ~salt-result)
               (= :multi (first ~salt-result)))
        (is (set (rest ~salt-result)) (eval ~salt-form))
        (when (and (not (and (vector? ~salt-result)
                             (= :skip (first ~salt-result))))
                   (not= :ignore ~salt-result))
          (is (= ~salt-result (eval (if (and (map? ~salt-form)
                                             (= (set (keys ~salt-form)) #{:code
                                                                          :alternate}))
                                      (:code ~salt-form)
                                      ~salt-form))))))
      (is (= ~tla-code (transpiler/transpile-single-form (if (and (map? ~salt-form)
                                                                  (= (set (keys ~salt-form)) #{:code
                                                                                               :alternate}))
                                                           (:code ~salt-form)
                                                           ~salt-form))))
      (output :desc ~desc)
      (output :tla-code (str "" ~tla-code "") ~checked?)
      (code-table-output ~salt-form
                         (when (and (not (nil? ~salt-result))
                                    (not (and (vector? ~salt-result)
                                              (#{:skip :multi} (first ~salt-result))))
                                    (not= :ignore ~salt-result))
                           ~salt-result)
                         ~tla-code
                         (when (not (= :? ~tla-result))
                           ~tla-result))))
  ([desc pre-salt-forms salt-form salt-result tla-code tla-result checked?]
   `(binding [*ns* (find-ns 'salt.test-suite)]
      (transpiler/reset)
      (let [base-text# (loop [result# ""
                              [form# & more#] ~pre-salt-forms]
                         (if form#
                           (recur (transpiler/transpile-form form#) more#)
                           result#))]
        (when (not= :ignore ~salt-result)
          (if (and (vector? ~salt-result)
                   (= :multi (first ~salt-result)))
            (is (set (rest ~salt-result)) (eval ~salt-form))
            (is (= ~salt-result (eval ~salt-form)))))
        (is (= ~tla-code
               (subs (transpiler/transpile-form ~salt-form)
                     (count base-text#)))))
      (output :desc ~desc)
      (output :tla-code (str "" ~tla-code "") ~checked?)
      (code-table-output ~salt-form (when (and (not (nil? ~salt-result))
                                               (not (and (vector? ~salt-result)
                                                         (#{:skip :multi} (first ~salt-result))))
                                               (not= :ignore ~salt-result))
                                      ~salt-result)
                         ~tla-code
                         (when (not (= :? ~tla-result))
                           ~tla-result)))))

(deftest test-primitives
  (section "Primitives")
  (check "Integers are represented the same in salt and TLA+:"
         '1
         1
         "1"
         "1"
         true)

  (check "Strings are sequences of characters"
         '"hello"
         "hello"
         "\"hello\""
         "\"hello"
         true)

  (check "Symbols are identifiers. For example \"x\" is a symbol in the following:"
         '(let [x 1]
            x)
         1
         "LET x == 1\nIN  x"
         "1"
         true)

  (check "Boolean literals:"
         'true
         true
         "TRUE"
         "TRUE"
         true)
  (check ""
         'false
         false
         "FALSE"
         "FALSE"
         true))

(deftest test-code-structures
  (section "Code Structure")
  (check "Let statements are used to establish bindings between symbols and values:"
         '(let [x 1
                y 2]
            (+ x y))
         3
         "LET x == 1\n    y == 2\nIN  x + y"
         "3"
         true)

  (check "Conditional statements are expressed as:"
         '(if true 100 0)
         100
         "IF  TRUE\nTHEN 100\nELSE 0"
         "100"
         true)

  (check "To perform many checks on a value use 'cond':"
         '(let [x 3]
            (cond
              (= x 1) true
              (= x 2) true
              (= x 3) 7
              :default false))
         7
         "LET x == 3\nIN  CASE\n    (x = 1) -> TRUE\n    [] (x = 2) -> TRUE\n    [] (x = 3) -> 7\n    [] OTHER -> FALSE\n    "
         "7"
         true))

(deftest test-arithmetic
  (section "Arithmetic")
  (check "Arithmetic on integers:"
         '(+ 1 2)
         3
         "1 + 2"
         "3"
         true)
  (check "Subtraction"
         '(- 3 2)
         1
         "3 - 2"
         "1"
         true)
  (check "Multiplication"
         '(* 3 2)
         6
         "3 * 2"
         "6"
         true)
  (check "Integer division"
         '(div 10 2)
         5
         "10 \\div 2"
         "5"
         true)
  (check "Integer division results are truncated"
         '(div 9 2)
         4
         "9 \\div 2"
         "4"
         true)
  (check "Compute the modulus of two numbers."
         '(mod* 10 3)
         1
         "10 % 3"
         "1"
         true)
  (check "Compute exponentiation"
         '(expt 2 3)
         8
         "2^3"
         "8"
         true)
  (check "Refer to the set of natural numbers. The clojure version of this uses a very small set of natural numbers by default.  NOTE: Nat is invoked as a function in clojure so that the upper limit can be dynamically bound, if necessary for testing."
         '(contains? (Nat) 2)
         true
         "2 \\in Nat"
         "TRUE"
         true))

(deftest test-spec
  (section "Specs")
  (check "Start a spec with a standard namespace declaration."
         '(ns Buffer
            (:require [salt.lang :refer :all]
                      [tlaplus.Naturals :refer :all]
                      [tlaplus.Sequences :refer :all]))
         "---------------------------- MODULE Buffer ----------------------------\nEXTENDS Naturals, Sequences\n\n"
         ""
         false)

  (check "Define the CONSTANTS, which serve as a sort of 'input' to the specification and define the scope of the model."
         '(CONSTANT Clients Servers Data)
         nil
         "CONSTANT Clients, Servers, Data\n\n"
         ""
         false)

  (check "Make assertions about constants"
         ['(CONSTANT Clients Servers Limit)
          '(def Clients #{})
          '(def Servers #{})
          '(def Limit 20)]
         '(ASSUME (and
                   (subset? Clients Servers)
                   (< Limit 100)))
         :ignore
         "ASSUME\n    /\\  Clients \\subseteq Servers\n    /\\  Limit < 100\n\n"
         ""
         false)

  (check "Define the variables that make up the state:"
         '(VARIABLE messages leaders)
         nil
         "VARIABLE messages, leaders\n\n"
         ""
         false)

  (check "Specify the initial state of variables. This is an addition to TLA+ which does not have an equivalent of the REQUIRE- keyword. NOTE: it is not possible to specify general predicates here."
         ['(VARIABLE messages leaders)]
         '(REQUIRE- [messages []
                     leaders #{}])
         true
         "/\\  messages = << >>\n/\\  leaders = {}"
         ""
         false)

  (check "To indicate allowed state transitions reference variable symbols with a prime suffix. This is an addition to TLA+ which does not have an equivalent to the ALLOW- keyword. NOTE: it is not possible to specify general predicates here."
         ['(VARIABLE messages leaders)]
         '(ALLOW- [messages' []
                   leaders' #{}])
         [:multi true false]
         "/\\  messages' = << >>\n/\\  leaders' = {}"
         ""
         false)

  (check "If there are multiple ALLOW- blocks in a single rule that need to be applied together then wrap them in an atomic- block. NOTE: There is no coresponding TLA+ identifier as the default behavior from TLA+ is to atomically apply all the state changes."
         ['(VARIABLE messages leaders)]
         '(atomic- (and (ALLOW- [messages' []])
                        (ALLOW- [leaders' #{}])))
         [:multi true false]
         "/\\  messages' = << >>\n/\\  leaders' = {}"
         ""
         false)

  (check "Indicate variables that are not changed"
         ['(VARIABLE messages leaders)]
         '(UNCHANGED [messages leaders])
         true
         "UNCHANGED << messages, leaders >>"
         ""
         false)

  (check "As a departure from TLA+, just the changed variables can be indicated instead. This implies that other variables are unchanged."
         ['(VARIABLE messages leaders)]
         '(CHANGED- [leaders])
         true
         "UNCHANGED << messages >>"
         ""
         false)

  (check "Include horizontal separator lines in the spec to delimit sections."
         '(line-)
         nil
         "--------------------------------------------------------------------------------\n"
         ""
         false)

  (check "Include comments in the spec."
         '(comment "this is a single line comment")
         nil
         "\n\\* this is a single line comment\n"
         ""
         true)

  (check "Comments can be multi-line"
         '(comment "this is a
multi line comment")
         nil
         "\n(*\nthis is a\nmulti line comment\n*)\n"
         ""
         true))

(deftest test-logic
  (section "Logic")
  (check "Use standard logic operators"
         '(and true false)
         false
         "/\\  TRUE\n/\\  FALSE"
         "FALSE"
         true)
  (check "The 'or' operator"
         '(or true false)
         true
         "\\/  TRUE\n\\/  FALSE"
         "TRUE"
         true)
  (check "Specify that if x is true then y must be true as well"
         '(=> true false)
         false
         "TRUE => FALSE"
         "FALSE"
         true)
  (check "Use the TLA+ <=> operator"
         '(<=> true false)
         false
         "TRUE <=> FALSE"
         "FALSE"
         true)

  (check "Check for equality"
         '(= 5 5)
         true
         "5 = 5"
         "TRUE"
         true)

  (check "Equality works on complex types"
         '(= #{"a" "b"}
             #{"b" "a"})
         true
         "{ \"a\", \"b\" } = { \"a\", \"b\" }"
         "TRUE"
         true)

  (check "Check for two items not being equal to each other"
         '(not= 1 2)
         true
         "1 # 2"
         "TRUE"
         true)

  (check "Use the standard inequality operators:"
         '[(< 1 2)
           (<= 1 1)
           (> 2 1)
           (>= 2 2)]
         [true true true true]
         "<< (1 < 2), (1 <= 1), (2 > 1), (2 >= 2) >>"
         "<<TRUE, TRUE, TRUE, TRUE>>"
         true)

  (check "Operate on an item that exists in a set"
         '(E [x #{1 2 3}]
             x)
         [:multi 1 2 3]
         "\\E x \\in { 1, 3, 2 } :\n    x"
         :?
         true)

  (check "Specify something is true for all items in a set"
         '(A [x #{1 2 3}]
             (> x 2))
         false
         "\\A x \\in { 1, 3, 2 } :\n    x > 2"
         :?
         true))

(deftest test-temporal-logic
  (section "Temporal Logic")
  (check "Say something is always true"
         ['(VARIABLE x y)
          '(defn Next [])
          '(def vars [x y])]
         '(always- (Next) vars)
         :ignore
         "[][Next]_vars"
         ""
         false)

  (check "Say something is eventually true"
         ['(VARIABLE x y)
          '(defn Done [])
          '(def vars [x y])]
         '(eventually- (Done))
         :ignore
         "<>Done"
         ""
         false)

  (check "Say that something being true leads to something else being true"
         ['(VARIABLE x y)
          '(defn P [])
          '(defn Q [])
          '(def vars [x y])]
         '(leads-to- P Q)
         :ignore
         "P ~> Q"
         ""
         false)

  (check "Specify weak fairness"
         ['(VARIABLE x y)
          '(defn Next [])
          '(def vars [x y])]
         '(WF vars (Next))
         :ignore
         "WF_vars(Next)"
         ""
         false)

  (check "Specify strong fairness"
         ['(VARIABLE x y)
          '(defn Next [])
          '(def vars [x y])]
         '(SF vars (Next))
         :ignore
         "SF_vars(Next)"
         ""
         false))

(deftest test-sets
  (section "Sets")
  (check "Set literals are defined as:"
         '#{1 2 3}
         #{1 3 2}
         "{ 1, 3, 2 }"
         "{1, 2, 3}"
         true)

  (check "A sequence of values is defined with range* Note that range* produces a set and is inclusive of the final value to match TLA+ semantics."
         '(range* 2 5)
         #{2 3 4 5}
         "2..5"
         "{2 3 4 5}"
         true)

  (check "Standard set operations come from the clojure.set namespace"
         '[(union #{1 2} #{2 3})
           (difference #{1 2} #{2 3})
           (intersection #{1 2} #{2 3})]
         [#{1 3 2}
          #{1}
          #{2}]
         "<< ({ 1, 2 } \\union { 3, 2 }), ({ 1, 2 } \\ { 3, 2 }), ({ 1, 2 } \\intersect { \n            3, 2 }) >>"
         "<<{1, 2, 3}, {1}, {2}>>"
         true)

  (check "Collapse many sets into one with UNION"
         '(UNION #{#{1 2} #{3 4} #{3 5}})
         #{1 2 3 4 5}
         "UNION { { 4, 3 }, { 3, 5 }, { 1, 2 } }"
         "{1, 2, 3, 4, 5}"
         true)

  (check "The cartesian product of two sets is computed by the 'X' operator"
         '(X #{1 2 3} #{"a" "b"})
         #{[1 "a"]
           [1 "b"]
           [2 "a"]
           [2 "b"]
           [3 "a"]
           [3 "b"]}
         "{ 1, 3, 2 } \\X { \"a\", \"b\" }"
         "{<<1, \"a\">>, <<1, \"b\">>, <<2, \"a\">>, <<2, \"b\">>, <<3, \"a\">>, <<3, \"b\">>}"
         true)

  (check "Check if a set is a subset of another"
         '(subset? #{1 3} #{1 2 3 4})
         true
         "{ 1, 3 } \\subseteq { 1, 4, 3, 2 }"
         "TRUE"
         true)

  (check "Define all of the sets that can be made from a set of values."
         '(SUBSET #{1 2 3})
         #{#{}
           #{1}
           #{2}
           #{3}
           #{1 2}
           #{1 3}
           #{2 3}
           #{1 2 3}}
         "SUBSET { 1, 3, 2 }"
         "{{}, {1}, {2}, {3}, {1, 2}, {1, 3}, {2, 3}, {1, 2, 3}}"
         true)

  (check "Check if a item is contained in a set."
         '(contains? #{1 2 3} 2)
         true
         "2 \\in { 1, 3, 2 }"
         "TRUE"
         true)

  (check "Filter the values in a set to be those matching a predicate"
         '(select (fn [x]
                    (> x 10)) #{5 15})
         #{15}
         "{ x \\in { 15, 5 } :\n    x > 10 }"
         "{15}"
         true)

  (check "Apply a function to all elements of a set"
         '(map* (fn [x]
                  (+ 1 x)) #{1 2})
         [:skip #{2 3}]
         "{ 1 + x :\n    x \\in { 1, 2 } }"
         "{2, 3}"
         true)

  (check "Compute the size of a set"
         '(Cardinality #{10 20})
         2
         "Cardinality( { 20, 10 } )"
         "2"
         true)

  (check "Use the TLA+ CHOOSE operator."
         '(CHOOSE [x #{1 2 3}]
                  (>= x 3))
         3
         "CHOOSE x \\in { 1, 3, 2 } : (x >= 3)"
         "3"
         true))

(deftest test-vectors
  (section "Vectors")
  (check "Clojure vector literals are TLA+ tuples:"
         '[1 "a"]
         [1 "a"]
         "<< 1, \"a\" >>"
         "<<1, \"a\">>"
         true)

  (check "Extract the first item from a vector:"
         '(first [1 "a"])
         1
         "Head(<< 1, \"a\" >>)"
         "1"
         true)

  (check "Extract a value by index. NOTE: the index starts with 1."
         '(get* [10 20 30] 2)
         20
         "<< 10, 20, 30 >>[2]"
         "20"
         true)

  (check "Produce a new vector containing all but the first item:"
         '(rest [1 "a"])
         ["a"]
         "Tail(<< 1, \"a\" >>)"
         "<<\"a\">>"
         true)

  (check "Combine the contents of two vectors into a new vector"
         '(into [1 "a"] [2])
         [1 "a" 2]
         "<< 1, \"a\" >> \\o << 2 >>"
         "<<1, \"a\", 2>>"
         true)

  (check "Combine two strings, which TLA+ treats as tuples:"
         '(str "hel" "lo")
         "hello"
         "\"hel\" \\o \"lo\""
         "\"hello\""
         true)

  (check "Compute the length of a vector or string"
         '[(count [1 "a"])
           (count "hello")]
         [2 5]
         "<< Len( << 1, \"a\" >> ), Len( \"hello\" ) >>"
         "<<2, 5>>"
         true)

  (check "Extract a subsequence by index from a vector"
         '(SubSeq [10 20 30 40] 2 3)
         [20 30]
         "SubSeq(<< 10, 20, 30, 40 >>, 2, 3)"
         "<<20, 30>>"
         true)

  (check "Filter out the values in a vector"
         '(SelectSeq (fn [x]
                       (> x 2)) [1 2 3 4])
         [3 4]
         "SelectSeq(<< 1, 2, 3, 4 >>, LAMBDA  x: (x > 2))"
         "<<3, 4>>"
         true)

  (check "Add an item to the end of a vector"
         '(conj [1 2] 3)
         [1 2 3]
         "Append(<< 1, 2 >>, 3)"
         "<<1, 2, 3>>"
         true)

  (check "Generate all possible vectors from a set of values."
         '(Seq #{100 200})
         #{[100]
           [100 200 200]
           [200 100 200]
           [200 200]
           [200 100 100]
           [100 200]
           []
           [100 100]
           [100 200 100]
           [200 200 200]
           [100 100 100]
           [200 200 100]
           [200]
           [200 100]
           [100 100 200]}
         "Seq( { 100, 200 } )"
         "Seq({100, 200})"
         true)

  (check "The idiom of calling 'every?*' with a set as a predicate translates into a corresponding TLA+ idiom for checking a TLA+ tuple."
         ['(defn CustomPredicate [])]
         '(every?* #{1 2 3 4 5} [1 3])
         true
         "<< 1, 3 >> \\in (Seq( { 1, 4, 3, 2, 5 } ))"
         "TRUE"
         true)

  (check "Compute all of the indexes present in a vector"
         '(DOMAIN [10 20 30])
         #{1 2 3}
         "DOMAIN << 10, 20, 30 >>"
         "1..3"
         true))

(deftest test-maps
  (section "Maps")

  ;; How to define a map literal with arbitrary keys?
  ;; What about using tuples as keys?

  (check "Use 'maps-' to generate all possible maps for a set of possible keys and a set of possible values."
         '(maps- #{10 20} #{100 200})
         #{{20 100
            10 200}
           {20 200
            10 100}
           {20 100
            10 100}
           {20 200
            10 200}}
         "[{ 20, 10 } -> { 100, 200 }]"
         "{ (10 :> 100 @@ 20 :> 100),
  (10 :> 100 @@ 20 :> 200),
  (10 :> 200 @@ 20 :> 100),
  (10 :> 200 @@ 20 :> 200) }"
         true)

  (check "Define a map via 'fm-'."
         '(fm- [a #{10 20}]
               30)
         {20 30
          10 30}
         "[a \\in { 20, 10 } |-> 30]"
         "(10 :> 30 @@ 20 :> 30)"
         true)

  (check "Use 'defm-' to define a map like with 'fm-', but assign the result a name."
         '(defm- MyMaps [a #{1 2}]
            (* a 10))
         :ignore
         "MyMaps == [a \\in { 1, 2 } |-> (a * 10)]\n\n"
         ""
         false)

  (check "Extract a value by key."
         '(get* (fm- [a #{10 20}]
                     (* a 10)) 10)
         100
         "[a \\in { 20, 10 } |-> (a * 10)][10]"
         "100"
         true))

(deftest test-records
  (section "Maps As Records")

  (check "Map literals whose keys are keywords become TLA+ records, which are a special type of a TLA+ function"
         '{:a 100
           :b 200}
         {:a 100
          :b 200}
         "[a |-> 100,\n b |-> 200]"
         "[a |-> 100, b |-> 200]"
         true)

  (check "Access the values in a TLA+ record."
         '(get* {:a 100
                 :b 200} :b)
         200
         "[a |-> 100,\n b |-> 200].b"
         "200"
         true)

  (check "Use 'DOMAIN' on maps to obtain the keys."
         '(DOMAIN {:a 100
                   :b 200})
         #{:a :b}
         "DOMAIN [a |-> 100,\n        b |-> 200]"
         "{\"a\", \"b\"}"
         true)

  (check "Produce a new TLA+ record with modified values, like assoc-in"
         '(EXCEPT {:a 1 :b 2 :c 3} [:b] 20)
         {:a 1
          :b 20
          :c 3}
         "[[a |-> 1,\n  b |-> 2,\n  c |-> 3] EXCEPT ![\"b\"] = 20]"
         "[a |-> 1, b |-> 20, c |-> 3]"
         true)
  (check "Produce a new nested TLA+ record with modified values, like assoc-in"
         '(EXCEPT {:a {:x 1 :y 10} :b {:x 2 :y 20} :c {:x 3 :y 30}} [:b :x] 200)
         {:a {:x 1
              :y 10}
          :b {:x 200
              :y 20}
          :c {:x 3
              :y 30}}
         "[[a |-> [x |-> 1,\n         y |-> 10],\n  b |-> [x |-> 2,\n         y |-> 20],\n  c |-> [x |-> 3,\n         y |-> 30]] EXCEPT ![\"b\"][\"x\"] = 200]"
         "[ a |-> [x |-> 1, y |-> 10],
  b |-> [x |-> 200, y |-> 20],
  c |-> [x |-> 3, y |-> 30] ]"
         true)

  (check "Produce a new TLA+ record with new values computed by lambda function, like update-in"
         {:code '(EXCEPT {:a {:x 1 :y 10} :b {:x 2 :y 20} :c {:x 3 :y 30}} [:b :x] #(* % 2))
          :alternate "(EXCEPT {:a {:x 1 :y 10}
         :b {:x 2 :y 20}
         :c {:x 3 :y 30}}
        [:b :x]
        #(* % 2))"}
         {:a {:x 1
              :y 10}
          :b {:x 4
              :y 20}
          :c {:x 3
              :y 30}}
         "[[a |-> [x |-> 1,\n         y |-> 10],\n  b |-> [x |-> 2,\n         y |-> 20],\n  c |-> [x |-> 3,\n         y |-> 30]] EXCEPT ![\"b\"][\"x\"] = @ * 2]"
         "[[
    a |-> [
        x |-> 1,
        y |-> 10],
    b |-> [
        x |-> 2,
        y |-> 20],
    c |-> [
        x |-> 3,
        y |-> 30]] EXCEPT ![\"b\"][\"x\"] = @ * 2]"
         true)

  (check "Produce a new TLA+ record with new values and with lambdas, like combining assoc-in and update-in"
         {:code '(EXCEPT {:a {:x 1 :y 10} :b {:x 2 :y 20} :c {:x 3 :y 30}}
                         [:b :x] #(* % 2)
                         [:a] "new")
          :alternate "(EXCEPT {:a {:x 1 :y 10}
         :b {:x 2 :y 20}
         :c {:x 3 :y 30}}
        [:b :x] #(* % 2)
        [:a] \"new\")"}
         {:a "new"
          :b {:x 4
              :y 20}
          :c {:x 3
              :y 30}}
         "[[a |-> [x |-> 1,\n         y |-> 10],\n  b |-> [x |-> 2,\n         y |-> 20],\n  c |-> [x |-> 3,\n         y |-> 30]] EXCEPT\n        ![\"b\"][\"x\"] = @ * 2,\n        ![\"a\"] = \"new\"]"
         "[a |-> \"new\", b |-> [x |-> 4, y |-> 20], c |-> [x |-> 3, y |-> 30]]"
         true)

  (check "Use 'maps-' to generate all possible TLA+ records for pairs of keys and value sets"
         '(maps- [:name #{"bob" "sue"}
                  :age #{10 20}])
         #{{:name "bob"
            :age 10}
           {:name "bob"
            :age 20}
           {:name "sue"
            :age 10}
           {:name "sue"
            :age 20}}
         "[name : { \"bob\", \"sue\" },\n age : { 20, 10 }]"
         "{[name |-> \"bob\", age |-> 10], [name |-> \"bob\", age |-> 20], [name |-> \"sue\", age |-> 10], [name |-> \"sue\", age |-> 20]}"
         true)

  (check "Define a map via 'fm-', will auto-convert to a TLA+ record."
         '(fm- [a #{"a" "b"}]
               30)
         {"a" 30, "b" 30}
         "[a \\in { \"a\", \"b\" } |-> 30]"
         "[a |-> 30, b |-> 30]"
         true))

(deftest test-tuples
  (section "Maps As Tuples")

  (check "Maps whose keys start with 1 and proceed in increments of 1 are treated as TLA+ tuples."
         '{1 100
           2 200}
         {1 100
          2 200}
         "<< 100, 200 >>"
         "<<100, 200>>"
         true)

  (check "Use 'DOMAIN' on maps that correspond to TLA+ tuples."
         '(DOMAIN {1 100
                   2 200})
         #{1 2}
         "DOMAIN << 100, 200 >>"
         "1..2"
         true)
  (check "Extract a value from a TLA+ tuple by index. NOTE: the index starts with 1."
         '(get* {1 100
                 2 200} 1)
         100
         "<< 100, 200 >>[1]"
         "100"
         true)

  (check "Define a map via 'fm-', will auto-convert to a TLA+ tuple"
         '(fm- [a #{1 2}]
               (* a 10))
         {1 10
          2 20}
         "[a \\in { 1, 2 } |-> (a * 10)]"
         "<<10, 20>>"
         true))

(deftest test-functions
  (section "Functions")
  (check "Define a function using fn. Depending on the context it will be transpiled to different forms."
         '[(SelectSeq (fn [x]
                        (> x 2)) [1 2 3 4])
           (map* (fn [x]
                   (+ 1 x)) #{1 2})]
         [[3 4]
          #{3 2}]
         "<< (SelectSeq(<< 1, 2, 3, 4 >>, LAMBDA  x: (x > 2))), (\n{ 1 + x :\n    x \\in { 1, 2 } }) >>"
         "<<<<3, 4>>, {2, 3}>>"
         true)
  (check "Define a new TLA+ operator with defn:"
         '(defn Add [x y]
            (+ x y))
         :ignore
         "Add( x, y ) ==\n    x + y\n\n"
         ""
         true)

  (check "Invoke a function as normal:"
         ['(defn Add [x y]
             (+ x y))]
         '(Add 1 2)
         :ignore
         "Add(1, 2)"
         "3"
         true)

  (check "Define a recursive function:"
         '(defn Add [x r]
            (if (> x 5)
              (Add (- x 1) (+ r 1))
              r))
         :ignore
         "RECURSIVE Add(_, _)\n\nAdd( x, r ) ==\n    IF  (x > 5)\n    THEN Add((x - 1), (r + 1))\n    ELSE r\n\n"
         ""
         true)

  (check "Define a higher-order function:"
         '(defn Work [f a b]
            (f a b))
         :ignore
         "Work( f(_, _), a, b ) ==\n    f(a, b)\n\n"
         ""
         true)

  (check "Define functions that take no arguments as usual:"
         ['(VARIABLE a b)]
         '(defn Work [] (union a b))
         :ignore
         "Work == a \\union b\n\n"
         ""
         false)

  (check "Invoke a function with no arguments."
         ['(defn Work [] (+ 1 2))]
         '(Work)
         :ignore
         "Work"
         ""
         false)

  (check "Define TLA+ operators that only rely on constants with def:"
         ['(CONSTANT A B)]
         '(def Work (union A B))
         :ignore
         "Work == A \\union B\n\n"
         ""
         false)

  (check "Reference a TLA+ operator that only relies on constants"
         ['(CONSTANT A B)
          '(def Work (union A B))]
         'Work
         :ignore
         "Work"
         ""
         false))

(defn produce-docs []
  (binding [*output-docs* true
            *output-debug* false]
    (test-primitives)
    (test-code-structures)
    (test-arithmetic)
    (test-logic)
    (test-spec)
    (test-temporal-logic)
    (test-sets)
    (test-vectors)
    (test-maps)
    (test-records)
    (test-tuples)
    (test-functions)))

(deftest write-docs []
  (let [doc-str (with-out-str (produce-docs))
        doc-template (slurp "resources/README.md.template")]
    (spit "README.md" (-> doc-template
                          (string/replace "@docs" doc-str)
                          (string/replace "@example-salt" (slurp "src/TwoPhase.clj"))
                          (string/replace "@example-tla" (slurp "resources/tla/TwoPhase.tla.expected"))))))

;; (run-tests)

