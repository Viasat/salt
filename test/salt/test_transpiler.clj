(ns salt.test-transpiler
  (:require [clojure.data :as data]
            [clojure.set :as set]
            [clojure.test :refer :all]
            [salt]
            [salt.lang :refer :all]
            [salt.transpiler :as transpiler]
            [tlaplus.Integers :refer :all]
            [tlaplus.Sequences :refer :all]))

(deftest test-allow-clauses
  (is (= ['p 100 'q 200]
         (clauses-to-bindings '((= p 100)
                                (= q 200)))))
  (is (= ['p' 100 'q' 200]
         (clauses-to-bindings '((= p' 100)
                                (= q' 200)))))
  (is (thrown-with-msg? RuntimeException #"all clauses need to reference prime or non-prime variables"
                        (clauses-to-bindings '((= p 100)
                                               (= q' 200)))))
  (is (thrown-with-msg? RuntimeException #"all clauses need to reference variables as first operand"
                        (clauses-to-bindings '((= 100 q)
                                               (= q' 200)))))
  (is (thrown-with-msg? RuntimeException #"Expected an equality predicate"
                        (clauses-to-bindings '((> q' 200))))))

;;

(VARIABLE M N)

(deftest test-basic-types
  (is (= "100"
         (transpiler/transpile-single-form '100)))
  (is (= "-100"
         (transpiler/transpile-single-form '-100)))
  (is (= "\"hello there\""
         (transpiler/transpile-single-form "hello there")))
  (is (= "TRUE"
         (transpiler/transpile-single-form 'true)))
  (is (= "FALSE"
         (transpiler/transpile-single-form 'false))))

(deftest test-symbols
  (transpiler/reset)
  (transpiler/transpile-form '(def Abc 100))
  (is (= "Abc == 100

Abc"
         (transpiler/transpile-form 'Abc))))

(deftest test-math
  (is (= "1 + 2"
         (transpiler/transpile-single-form '(+ 1 2))))
  (is (= "1 - 2"
         (transpiler/transpile-single-form '(- 1 2))))
  (is (= "1 * 2"
         (transpiler/transpile-single-form '(* 1 2))))
  (is (= "1 \\div 2"
         (transpiler/transpile-single-form '(div 1 2))))
  (is (= "1 = 2"
         (transpiler/transpile-single-form '(= 1 2))))
  (is (= "1 % 2"
         (transpiler/transpile-single-form '(mod* 1 2))))
  (is (= "3^2"
         (transpiler/transpile-single-form '(expt 3 2)))))

;; test defs

(deftest test-defs
  (def X1 5)
  (is (= 5 X1))
  (is (= "X1 == 5

"  (transpiler/transpile-single-form '(def X1 5)))))

(deftest test-defns
  (def X1 5)
  (defn Y [x] (= x X1))
  (is (Y 5))
  (is (not (Y 6)))
  (transpiler/reset)
  (transpiler/transpile-form '(def X1 5))
  (is (= "X1 == 5

Y( x ) ==
    x = X1

"  (transpiler/transpile-form '(defn Y [x] (= x X1))))))

(deftest test-defn-called-with-no-args
  (transpiler/reset)
  (transpiler/transpile-form '(defn Y [] (+ 1 2)))
  (is (= "Y == 1 + 2

Y"
         (transpiler/transpile-form '(Y)))))

(deftest test-defs-in-set
  (def X1 5)
  (is (= #{X1 10} #{5 10}))
  (transpiler/reset)
  (transpiler/transpile-form '(def X1 5))
  (is (= "X1 == 5

{ X1, 10 } = { 5, 10 }"
         (transpiler/transpile-form '(= #{X1 10} #{5 10})))))

(deftest test-higher-order-functions
  (transpiler/reset)
  (transpiler/transpile-form '(defn Sum [a b] (+ a b)))
  (transpiler/transpile-form '(defn Inc [a] (+ 1 a)))
  (transpiler/transpile-form '(defn F [G] (+ 3 (G 1 2))))
  (transpiler/transpile-form '(defn H [I] (+ 4 (I 5))))
  (is (= "Sum( a, b ) ==
    a + b

Inc( a ) ==
    1 + a

F( G(_, _) ) ==
    3 + G(1, 2)

H( I(_) ) ==
    4 + I(5)

J( K(_), L(_, _) ) ==
    H(K) + F(L)

"
         (transpiler/transpile-form '(defn J [K L] (+ (H K) (F L)))))))

(deftest test-fn
  (is (= "LAMBDA  x, y: (x + y)"
         (transpiler/transpile-single-form '(fn [x y] (+ x y))))))

(deftest test-recursive-function
  (is (= "RECURSIVE SetToTuple(_)

SetToTuple( s ) ==
    IF  (s = {})
    THEN << >>
    ELSE \n        LET x == CHOOSE x \\in s : TRUE
        IN  << x >> \\o SetToTuple(s \\ {x})

"
         (transpiler/transpile-single-form '(defn SetToTuple [s]
                                              (if (= s #{})
                                                []
                                                (let [x (CHOOSE [x s]
                                                                true)]
                                                  (into [x] (SetToTuple (difference s #{x}))))))))))

;; test logic

(deftest test-and
  (is (= "/\\  TRUE
/\\  1 = 2"
         (transpiler/transpile-single-form '(and true (= 1 2))))))

(deftest test-or
  (is (= "\\/  TRUE
\\/  1 = 2"
         (transpiler/transpile-single-form '(or true (= 1 2))))))

(deftest test-inequalities
  (is (= "1 > 2"
         (transpiler/transpile-single-form '(> 1 2))))
  (is (= "1 >= 2"
         (transpiler/transpile-single-form '(>= 1 2))))
  (is (= "1 < 2"
         (transpiler/transpile-single-form '(< 1 2))))
  (is (= "1 <= 2"
         (transpiler/transpile-single-form '(<= 1 2)))))

(deftest test-let
  (is (= "LET x == 1
IN  x = 2"
         (transpiler/transpile-single-form '(let [x 1]
                                              (= x 2))))))

(deftest test-if
  (is (= "IF  (1 = 2)
THEN 3
ELSE 4"
         (transpiler/transpile-single-form '(if (= 1 2)
                                              3
                                              4)))))

(deftest test-case
  (is (= "LET x == 3
IN  CASE
    (x = 1) -> TRUE
    [] (x = 2) -> TRUE
    [] (x = 3) -> 7
    [] OTHER -> FALSE
    "
         (transpiler/transpile-single-form '(let [x 3]
                                              (cond
                                                (= x 1) true
                                                (= x 2) true
                                                (= x 3) 7
                                                :default false))))))

(deftest test-=>
  (is (= "TRUE => FALSE"
         (transpiler/transpile-single-form '(=> true false)))))

(deftest test-<=>
  (is (= "TRUE <=> FALSE"
         (transpiler/transpile-single-form '(<=> true false)))))

(deftest test-E
  (is (= "\\E y \\in (2..10) :
    5 = y"
         (transpiler/transpile-single-form '(E [y (range* 2 10)]
                                               (= 5 y))))))

(deftest test-A
  (is (= "\\A f \\in {10} :
    /\\  f > 2
    /\\  f < 4"
         (transpiler/transpile-single-form '(A [f #{10}]
                                               (and (> f 2)
                                                    (< f 4)))))))

;;

(deftest test-nested-expressions
  (is (= "/\\  TRUE
/\\  \\/  2 = (3 + (3 - (3 + 4)))
/\\  \\/  (1 + 2) < (3 * (1 + 2))
    \\/  (3 + 0) > (1 + 2)
    \\/  (2 - (2 + 3)) <= (3 \\div 4)
    \\/  (2 + 3) >= 3
    \\/  3 < ((3^4) + 4)"
         (transpiler/transpile-single-form '(and true
                                                 (or (= 2 (+ 3 (- 3 (+ 3 4)))))
                                                 (or (< (+ 1 2) (* 3 (+ 1 2)))
                                                     (> (+ 3 0) (+ 1 2))
                                                     (<= (- 2 (+ 2 3)) (div 3 4))
                                                     (>= (+ 2 3) 3)
                                                     (< 3 (+ (expt 3 4) 4))))))))

;; tla+ tuples (i.e. clojure vectors)

(deftest test-tuples
  (is (= "<< >>"
         (transpiler/transpile-single-form '[])))
  (is (= "<< 10, 20, 30 >>"
         (transpiler/transpile-single-form '[10 20 30])))
  (is (= "Head(<< 10, 20, 30 >>)"
         (transpiler/transpile-single-form '(first [10 20 30]))))
  (is (= "Tail(<< 10, 20, 30 >>)"
         (transpiler/transpile-single-form '(rest [10 20 30]))))
  (is (= "Append(<< 10, 20, 30 >>, 40)"
         (transpiler/transpile-single-form '(conj [10 20 30] 40))))
  (is (= "<< 10, 20, 30 >> \\o << 40, 50 >>"
         (transpiler/transpile-single-form '(into [10 20 30] [40 50]))))
  (is (= "Len( << 10, 20, 30 >> )"
         (transpiler/transpile-single-form '(count [10 20 30]))))
  (is (= "DOMAIN << 10, 20, 30 >>"
         (transpiler/transpile-single-form '(DOMAIN [10 20 30])))))

(deftest test-cartesian-product
  (is (= #{[2 "b"]
           [2 "c"]
           [2 "a"]
           [1 "c"]
           [1 "a"]
           [1 "b"]}
         (X [1 2] ["a" "b" "c"])))
  (is (= #{[2 2]
           [2 3]
           [1 3]
           [1 2]}
         (X [1 2] [2 3])))
  (is (= "<< 1, 2 >> \\X << \"a\", \"b\", \"c\" >>"
         (transpiler/transpile-single-form '(X [1 2] ["a" "b" "c"])))))

(deftest test-select-seq
  (is (= [3 4]
         (SelectSeq (fn [x]
                      (> x 2)) [1 2 3 4])))
  (is (= "SelectSeq(<< 1, 2, 3, 4 >>, LAMBDA  x: (x > 2))"
         (transpiler/transpile-single-form '(SelectSeq (fn [x]
                                                         (> x 2)) [1 2 3 4])))))

(deftest test-sub-seq
  (is (= [20 30]
         (SubSeq [10 20 30 40] 2 3)))
  (is (= "SubSeq(<< 10, 20, 30, 40 >>, 2, 3)"
         (transpiler/transpile-single-form '(SubSeq [10 20 30 40] 2 3)))))

;; tla+ structures (i.e. clojure maps)

(deftest test-structures
  (is (= "[]"
         (transpiler/transpile-single-form '{})))
  (is (= "[a |-> 10]"
         (transpiler/transpile-single-form '{:a 10})))
  (is (= "[a |-> 10,
 b |-> 20]"
         (transpiler/transpile-single-form '{:a 10
                                             :b 20})))
  (is (= "[a |-> 10,
 b |-> [c |-> 20,
        d |-> << 30, 40 >>,
        e |-> { 60, 50 }]]"
         (transpiler/transpile-single-form '{:a 10
                                             :b {:c 20
                                                 :d [30 40]
                                                 :e #{50 60}}}))))

(deftest test-generate-set-of-maps
  (is (= "[a : {1},
 b : { 4, 3 },
 c : { 6, 5 }]"
         (transpiler/transpile-single-form '(maps- [:a #{1}
                                                    :b #{3 4}
                                                    :c #{5 6}])))))

(deftest test-functions
  (defm- Foo [s #{1 2 3}]
    (* s s))
  (is (= {1 1
          3 9
          2 4}
         Foo))
  (transpiler/reset)
  (transpiler/transpile-form '(defm- Foo [s #{1 2 3}]
                                (* s s)))
  (transpiler/transpile-form '(def G (get* Foo 2)))
  (is (= "Foo == [s \\in { 1, 3, 2 } |-> (s * s)]

G == Foo[2]

H == DOMAIN Foo\n\n"
         (transpiler/transpile-form '(def H (DOMAIN Foo))))))

(deftest test-anonymous-functions
  (is (= {1 1
          3 9
          2 4}
         (fm- [s #{1 2 3}]
              (* s s))))

  (is (= "[s \\in { 1, 3, 2 } |-> (s * s)]"
         (transpiler/transpile-single-form '(fm- [s #{1 2 3}]
                                                 (* s s))))))

(deftest test-access-tuple-as-map
  (is (= "<< 100, 200, 300 >>[1]"
         (transpiler/transpile-single-form '(get* [100 200 300] 1)))))

(deftest test-except
  (is (= {:a 1
          :b 20
          :c 3}
         (EXCEPT {:a 1 :b 2 :c 3} [:b] 20)))
  (is (= {:a 1
          :b 4
          :c 3}
         (EXCEPT {:a 1 :b 2 :c 3} [:b] #(* % 2))))
  (is (= {:a {:b {:c 4}}}
         (EXCEPT {:a {:b {:c 3}}} [:a :b :c] 4)))
  (is (= {:a {:b {:c 6}}}
         (EXCEPT {:a {:b {:c 3}}} [:a :b :c] #(* % 2))))
  (is (= {:a {:b 200
              :c 202}
          :x 306}
         (EXCEPT {:a {:b 100
                      :c 101}
                  :x 102}
                 [:a :b] 200
                 [:a :c] #(* 2 %)
                 [:x] #(* 3 %))))
  (is (= "[[a |-> 1,
  b |-> 2,
  c |-> 3] EXCEPT ![\"b\"] = 20]"
         (transpiler/transpile-single-form '(EXCEPT {:a 1 :b 2 :c 3} [:b] 20))))

  (is (= "[[a |-> 1,
  b |-> 2,
  c |-> 3] EXCEPT ![\"b\"] = @ * 2]"
         (transpiler/transpile-single-form '(EXCEPT {:a 1 :b 2 :c 3} [:b] #(* % 2)))))

  (is (= "[[a |-> [b |-> [c |-> 3]]] EXCEPT ![\"a\"][\"b\"][\"c\"] = 4]"
         (transpiler/transpile-single-form '(EXCEPT {:a {:b {:c 3}}} [:a :b :c] 4))))

  (is (=  "[[a |-> [b |-> [c |-> 3]]] EXCEPT ![\"a\"][\"b\"][\"c\"] = @ * 2]"
          (transpiler/transpile-single-form '(EXCEPT {:a {:b {:c 3}}} [:a :b :c] #(* % 2)))))

  (is (= "[[a |-> [b |-> 100,
         c |-> 101],
  x |-> 102] EXCEPT
        ![\"a\"][\"b\"] = 200,
        ![\"a\"][\"c\"] = 2 * @,
        ![\"x\"] = 3 * @]"
         (transpiler/transpile-single-form '(EXCEPT {:a {:b 100
                                                         :c 101}
                                                     :x 102}
                                                    [:a :b] 200
                                                    [:a :c] #(* 2 %)
                                                    [:x] #(* 3 %))))))

(deftest test-generate-set-of-maps-from-sets
  (is (= #{[4 3]
           [3 3]
           [3 4]
           [4 4]}
         (maps- #{1 2} #{3 4})))
  (is (= "[{ 1, 2 } -> { 4, 3 }]"
         (transpiler/transpile-single-form '(maps- #{1 2} #{3 4})))))

(deftest test-map-as-tuple
  ;; if a map has keys from 1 to n with no gaps then render it a tuple
  (is (= "<< 100, 200, 300 >>"
         (transpiler/transpile-single-form '{1 100 2 200 3 300})))

  (is (= "[1 |-> 100,
 3 |-> 300]"
         (transpiler/transpile-single-form '{1 100 3 300}))))

;; tla+ sets (i.e. clojure sets)

(deftest test-sets
  (is (= "{}"
         (transpiler/transpile-single-form '#{})))
  (is (= "{10}"
         (transpiler/transpile-single-form '#{10})))
  (is (or (= "{ 10, 20 }"
             (transpiler/transpile-single-form '#{10 20}))
          (= "{ 20, 10 }"
             (transpiler/transpile-single-form '#{10 20}))))
  (is (= "{ \"a\", \"b\" }"
         (transpiler/transpile-single-form '#{"a" "b"})))
  (is (= "10..100"
         (transpiler/transpile-single-form '(range* 10 100))))
  (is (or (= "{ {30}, { 20, 10 } }"
             (transpiler/transpile-single-form '#{#{10 20} #{30}}))
          (= "{ {30}, { 10, 20 } }"
             (transpiler/transpile-single-form '#{#{10 20} #{30}}))
          (= "{ { 20, 10 }, {30} }"
             (transpiler/transpile-single-form '#{#{10 20} #{30}}))
          (= "{ { 10, 20 }, {30 }"
             (transpiler/transpile-single-form '#{#{10 20} #{30}}))))
  (is (= "{[a |-> 10,
  b |-> << 20, 30 >>]}"
         (transpiler/transpile-single-form '#{{:a 10
                                               :b [20 30]}})))

  (is (= "20 \\in {10}"
         (transpiler/transpile-single-form '(contains? #{10} 20))))
  (is (= "20 \\notin {10}"
         (transpiler/transpile-single-form '(not (contains? #{10} 20)))))
  (is (= "{10} \\subseteq {20}"
         (transpiler/transpile-single-form '(subset? #{10} #{20}))))

  (is (subset? #{10} #{10}))
  (is (not (subset-proper? #{10} #{10})))
  (is (superset? #{10} #{10}))
  (is (not (superset-proper? #{10} #{10})))

  (is (subset? #{10} #{10 20}))
  (is (subset-proper? #{10} #{10 20}))
  (is (not (superset? #{10} #{10 20})))
  (is (not (superset-proper? #{10} #{10 20})))

  (is (not (subset? #{10 20} #{10})))
  (is (not (subset-proper? #{10 20} #{10})))
  (is (superset? #{10 20} #{10}))
  (is (superset-proper? #{10 20} #{10}))

  (is (= "{10} \\subset {20}"
         (transpiler/transpile-single-form '(subset-proper? #{10} #{20}))))
  (is (= "{10} \\supseteq {20}"
         (transpiler/transpile-single-form '(superset? #{10} #{20}))))
  (is (= "{10} \\supset {20}"
         (transpiler/transpile-single-form '(superset-proper? #{10} #{20}))))
  (is (= "{10} \\union {20}"
         (transpiler/transpile-single-form '(union #{10} #{20}))))
  (is (= "{10} \\intersect {20}"
         (transpiler/transpile-single-form '(intersection #{10} #{20}))))
  (is (= "{10} \\ {20}"
         (transpiler/transpile-single-form '(difference #{10} #{20}))))
  (is (= "SUBSET {10}"
         (transpiler/transpile-single-form '(SUBSET #{10}))))
  (is (= "UNION { {10}, {20} }"
         (transpiler/transpile-single-form '(UNION #{#{10} #{20}}))))
  (is (= "Cardinality( {10} )"
         (transpiler/transpile-single-form '(Cardinality #{10})))))

(deftest test-filter-set
  (is (= "{ x \\in { 15, 5 } :
    x > 10 }"
         (transpiler/transpile-single-form '(select (fn [x]
                                                      (> x 10)) #{5 15})))))

(deftest test-map-set
  (is (= "{ x + 1 :
    x \\in { 15, 5 } }"
         (transpiler/transpile-single-form '(map* (fn [x]
                                                    (+ x 1)) #{5 15})))))

(deftest test-choose
  (is (= "CHOOSE x \\in {10} : (x + 10)"
         (transpiler/transpile-single-form '(CHOOSE [x #{10}]
                                                    (+ x 10))))))

(deftest test-map-constants
  (transpiler/reset)
  (transpiler/transpile-form '(def S1 #{10}))
  (transpiler/transpile-form '(def S2 (range* 2 20)))
  (is (= "S1 == {10}

S2 == 2..20

{ x + (3 - x) :
    x \\in (S1 \\union S2) }"
         (transpiler/transpile-form '(map* (fn [x]
                                             (+ x (- 3 x))) (union S1 S2))))))

;;

(comment
  (comment
    "this module is for doing important stuff this module is for doing important stuff this module is
for doing important stuff this module is for doing important stuff this module is for doing
important stuff this module is for doing important stuff ")

  (comment
    "this is a short comment")

  (CONSTANT [Files #{3 5 7}
             LastFile 12
             TargetFiles #{3}])

  (defn Prime [x]
    (and (> x 1)
         (not (E y in (range* 2 (- x 1))
                 (= (mod* x y) 0)))))

  (ASSUME (and
           (A f in Files
              (Prime f))
           (subset? TargetFiles Files)))

  (VARIABLE
   (comment
     "this is a
multiline comment
line 3")
   validFiles invalidFiles reportedFiles cacheMessages recentWriteFiles auditStarted
   auditEnded cacheState)

  (def FirstFile 0)

  (def BadFileHash 2)

  (def FileHashes (union Files #{BadFileHash}))

  (def CacheReportsRequestMessageType "cacheReportsRequest")
  (def CacheReportMessageType "cacheReport")

  (defn SendMultipleToCache [ms]
    (BIND [cacheMessages' (into cacheMessages ms)]))

  (defn CacheReportMessage [f h]
    {:type CacheReportMessageType
     :file f
     :hash h})

  (defn ValidCacheReportMessage [m]
    (and (= (:type m) CacheReportMessageType)
         (comment "check something here")
         (contains? Files (:file m))
         (contains? FileHashes (:hash m))))

  (defn AllReportedFiles []
    (union reportedFiles #{FirstFile LastFile}))

  (comment "This is the main entry point")

  (defn Init []
    (BIND [validFiles #{}
           invalidFiles #{}
           reportedFiles #{}]))

  (defn SetToTuple [s]
    (if (= s #{})
      []
      (let [x (CHOOSE [x s]
                      true)]
        (into [x] (SetToTuple (difference s #{x}))))))

  (defn TupleToSet [t]
    (if (= t [])
      #{}
      (let [x (first t)]
        (union #{x} (TupleToSet (rest t))))))

  (defn CacheFileHashSingle [f]
    (if (contains? validFiles f)
      f
      (if (contains? invalidFiles f)
        BadFileHash
        1)))

  (defn CacheFileHash [fs]
    (if (= fs #{})
      1
      (let [f (CHOOSE [f fs]
                      true)]
        (* (CacheFileHashSingle f) (CacheFileHash (difference fs #{f}))))))

  (defn CachedFiles []
    (union validFiles invalidFiles))

  (defn FilesBetween [fs f1 f2]
    (select (fn [f]
              (and (>= f f1)
                   (<= f f2))) fs))

  (defn CacheFileHashBetween [f1 f2]
    (CacheFileHash (FilesBetween (CachedFiles) f1 f2)))

  (defn WriteFile [f]
    (BIND [recentWriteFiles' (if auditStarted
                               (union recentWriteFiles #{f})
                               recentWriteFiles)]))

  (defn FileAdded []
    (and (not auditEnded)
         (E f in Files
            (and
             (or
              (BIND [validFiles' (union validFiles #{f})
                     invalidFiles' (difference invalidFiles #{f})])
              (BIND [invalidFiles' (union invalidFiles #{f})
                     validFiles' (difference validFiles #{f})]))
             (WriteFile f)))
         (CHANGED- #{validFiles invalidFiles})
         (CHANGED- #{invalidFiles})
         (CHANGED- #{cacheState})))

  (defn Foo []
    (and (> (count cacheMessages) 2)
         (map* (fn [f]
                 (CacheReportMessage f (CacheFileHashSingle f)))
               (FilesBetween CachedFiles f1 f2))
         (FileHashes)
         (let [c 20]
           (BIND [cacheState' (EXCEPT cacheState [c] {:type "hello"})]))))

  (defn Invariants []
    (A c n validFiles (= (:state (get* cacheState c)) 20)))

  (defn Next []
    (or (FileAdded)
        (WriteFile)))

  (defn Spec [] (and (Init)
                     (leads-to- (eventually- (always- (Next) vars)) (+ 2 3))))

  (defn Baz []
    (and (= 2 3)
         (BIND [cacheState' 4
                validFiles' 5]))))

(deftest test-defm-2 []
  (is (= "InPortRange == [r \\in { << 4, 5, 6 >>, << 1, 2, 3 >> }, p \\in (3..5) |-> 
    /\\  r[1] <= p\n    /\\  p <= r[2]]

"
         (transpiler/transpile-single-form '(defm- InPortRange [r #{[1 2 3] [4 5 6]}
                                                                p (range* 3 5)]
                                              (and (<= (get* r 1) p)
                                                   (<= p (get* r 2))))))))

(deftest test-Seq
  (is (= "UNION
{ [(1..n) -> { \"a\", \"b\" }] :
    n \\in { 1, 2 } }")
      (transpiler/transpile-single-form '(UNION (map* (fn [n] (maps- (range* 1 n) #{"a" "b"})) #{1 2}))))

  (is (= #{["b"]
           []
           ["a" "a"]
           ["a" "b"]
           ["b" "b"]
           ["a"]
           ["b" "a"]}
         (binding [*Nat* 2]
           (Seq #{"a" "b"}))))
  (is (= "Seq( { \"a\", \"b\" } )"
         (transpiler/transpile-single-form '(Seq #{"a" "b"})))))

(deftest test-every?*
  (is (thrown-with-msg? RuntimeException #"unsupported types for every"
                        (every?* #{1 2 3 4 5} #{1 3})))
  (is (= "<< 1, 3 >> \\in (Seq( { 1, 4, 3, 2, 5 } ))"
         (transpiler/transpile-single-form '(every?* #{1 2 3 4 5} [1 3])))))

(deftest test-tuple-to-seq
  (is (= "s == << 10, 2, 30, 4 >>\n\n{ s[i] :\n    i \\in DOMAIN s }"
         (do (transpiler/reset)
             (transpiler/transpile-form '(def s [10 2 30 4]))
             (transpiler/transpile-form '(map* (fn [i] (get* s i)) (DOMAIN s)))))))

;; test full specs

(defn- expected-file-name [spec-name]
  (str "resources/tla/" spec-name ".tla.expected"))

(defn- actual-file-name [spec-name]
  (str "resources/tla/" spec-name ".tla.actual"))

(defn accept [spec-name]
  (spit (expected-file-name spec-name) (slurp (actual-file-name spec-name))))

(defn check-spec [spec-name]
  (spit (actual-file-name spec-name) (salt/transpile (str "test/" spec-name ".clj")))
  (is (= (slurp (expected-file-name spec-name))
         (slurp (actual-file-name spec-name)))))

(defn- accept-all []
  (accept "FinalMatch")
  (accept "TwoPhase"))

(deftest test-specs []
  (check-spec "FinalMatch")
  (check-spec "TwoPhase"))

;; (accept-all)
;; (run-tests)
