(ns salt.test-seval
  (:require [clojure.test :refer :all]
            [salt.lang :refer :all]
            [salt.seval :as seval]
            [tlaplus.FiniteSets :refer :all]
            [tlaplus.Integers :refer :all]
            [tlaplus.Sequences :refer :all]))

(use-fixtures :each (seval/namespace-fixture 'salt.test-seval))

(deftest test-0
  (is (= [false false false false true true]
         [(#'seval/expr-with-free-vars? (seval/make-context '[a']) 100)
          (#'seval/expr-with-free-vars? (seval/make-context '[a']) 'a)
          (#'seval/expr-with-free-vars? (seval/make-context '[a']) [])
          (#'seval/expr-with-free-vars? (seval/make-context '[a']) '(+ a [:d {:a [1 2 #{"r" a}]}]))
          (#'seval/expr-with-free-vars? (seval/make-context '[a']) 'a')
          (#'seval/expr-with-free-vars? (seval/make-context '[a']) '(+ a [:d {:a [1 2 #{"r" a'}]}]))]))

  (is (= '(or (let [x 1] (and false (= x' 1)))
              (let [x 3] (and true (= x' 3)))
              (let [x 2] (and true (= x' 2))))
         (#'seval/expand-E (seval/make-context ['x'] {})
                           '(E [x #{1 3 2}] (and (>= x 2) (= x' x)))))))

(deftest test-1
  (is (= '(or false (> q' 20)))
      (let [m {'x 1
               'a 20}]
        (->> '(or (let [a 9]
                    (> x a))
                  (> q' a))
             (seval/seval* (seval/make-context '[q'] m)))))

  (is (= '(and z'
               (<= q' (+ 300 t')))
         (->> '(and a
                    (or b z')
                    (or b
                        (<= q' (+ 100 200 t'))))
              (seval/seval* (seval/make-context '[z' t' q']
                                                '{a true
                                                  b false}))
              seval/simplify)))

  (is (= '(or (let [x 1] false)
              (let [x 3] true)
              (let [x 2] true))
         (->> '(E [x s]
                  (>= x 2))
              (seval/seval* (seval/make-context [] '{s #{1 2 3}}))
              seval/simplify)))

  (is (= '(or (and (= x' 2)
                   (= y' (+ 201 x')))
              (and (= x' 3)
                   (= y' (+ 201 x'))))
         (let [m '{a true
                   b false
                   x 100
                   y 200
                   s #{1 2 3}}]
           (->> '(and (E [x s]
                         (and (>= x 2)
                              (= x' x)
                              (and (= y' (+ 1 y x')))))
                      (or a
                          b
                          (or c'
                              (> x 9999))))
                (seval/seval* (seval/make-context '[x' y' c'] m))
                seval/simplify
                (seval/seval* (seval/make-context '[x' y' c'] m))
                seval/collapse-and-or))))

  (is (= '(and (>= 201 x')
               (>= 203 x')
               (>= 202 x'))
         (let [m '{s #{1 2 3}
                   x 100}]
           (->> '(A [x s]
                    (and (> x 0)
                         (>= (+ x 200) x')))
                (seval/seval* (seval/make-context '[x'] m))
                seval/simplify))))

  (is (= '[true
           z'
           (or z'
               (or q' p'))]
         [(seval/simplify '(or false z' true))
          (seval/simplify '(and true z'))
          (seval/simplify '(or (or false z') (or q' p')))]))

  (is (= false
         (let [m '{foo (fn [r]
                         r)}]
           (->> '(foo false)
                (seval/seval* (seval/make-context  [] m))))))

  (is (= '(or (= 200 (+ 101 x'))
              (> false (+ 50 z'))
              10
              t')
         (let [m '{x 100
                   p true
                   q false
                   r 50
                   foo (fn [r]
                         (or r
                             t'
                             (boo p q)))
                   boo (fn [a b]
                         (and p (> q (+ r z'))))}]
           (->> '(or (= 200 (+ 1 x x'))
                     (foo 10))
                (seval/seval* (seval/make-context '[x' t' z'] m))
                seval/simplify
                seval/collapse-and-or))))

  (is (= false
         (let [m {'x 100
                  'p true
                  'q false
                  'r false
                  'foo '(fn [r]
                          (or r
                              (boo p q)))
                  'boo '(fn [a b]
                          (and a b))}]
           (->> '(foo false)
                (seval/seval* (seval/make-context [] m))))))

  (is (= 3
         (let [m '{Add (fn [x y]
                         (+ x y))
                   ApplyF (fn [f x y]
                            (f x y))
                   CallF (fn []
                           (ApplyF Add 1 2))}]
           (->> '(CallF)
                (seval/seval* (seval/make-context [] m))))))

  (is (= 21
         (let [m '{ApplyF (fn [f x y]
                            (f x y))
                   CallF (fn []
                           (ApplyF (fn [a b]
                                     (+ a b))
                                   1
                                   20))}]
           (->> '(CallF)
                (seval/seval* (seval/make-context [] m))))))

  (is (= 31
         (let [m '{ApplyF (fn [f x y]
                            (f x y))
                   CallF (fn []
                           (let [g 30]
                             (ApplyF (fn [a b]
                                       (+ a b))
                                     1
                                     g)))}]
           (->> '(CallF)
                (seval/seval* (seval/make-context [] m))))))
  (is (= 35
         (let [m '{ApplyF (fn [f x y]
                            (f x y))
                   CallF (fn []
                           (let [g 30]
                             (ApplyF (fn [a b]
                                       (+ a b g))
                                     1
                                     4)))}]
           (->> '(CallF)
                (seval/seval* (seval/make-context [] m))))))

  (is (= 41
         (let [m '{ApplyF (fn [f x y]
                            (f x y))
                   CallF (fn []
                           (A [g #{40}]
                              (ApplyF (fn [a b]
                                        (+ a b))
                                      1
                                      g)))}]
           (->> '(CallF)
                (seval/seval* (seval/make-context [] m))))))

  (is (= 'q'
         (let [m {'x 1}]
           (->> '(if (> x 5)
                   p'
                   q')
                (seval/seval* (seval/make-context '[p' q'] m))
                seval/simplify))))

  (is (= false
         (let [m {'x 1}]
           (->> '(cond (> x 5) p'
                       (> x 10) q')
                (seval/seval* (seval/make-context '[p' q'] m))
                seval/simplify))))

  (is (= '(and (> q' 3)
               (> q' 1)
               (> q' 6)
               (> q' 4)
               (> q' 3)
               (> q' 8))
         (let [m {}]
           (->> '(and (> q' (+ 1 2))
                      (> q' (- 2 1))
                      (> q' (* 2 3))
                      (> q' (div 8 2))
                      (> q' (mod* 12 9))
                      (> q' (expt 2 3)))
                (seval/seval* (seval/make-context '[q'] m))))))

  (is (= '(or (let [x 0] false)
              (let [x 1] false)
              (let [x 3] (> x' 3))
              (let [x 2] (> x' 2)))
         (let [m {}]
           (->> '(E [x (Nat)]
                    (and (> x 1)
                         (> x' x)))
                (seval/seval* (seval/make-context '[x'] m))
                seval/simplify))))

  (is (= '(and (=> true x')
               (<=> true x'))
         (let [m {'x true}]
           (->> '(and (=> x x')
                      (<=> x x'))
                (seval/seval* (seval/make-context '[x'] m))))))

  (is (= '(and false
               (not= 100 x'))
         (let [m {'x 100}]
           (->> '(and (not= (> x 5))
                      (not= x x'))
                (seval/seval* (seval/make-context '[x'] m))))))

  (is (= '(and (= p' 20)
               (= x' 100)
               (= y' 200))
         (let [m {'x 100
                  'y 200}]
           (->> '(and
                  (= p' 20)
                  (UNCHANGED [x y]))
                (seval/seval* (seval/make-context '[x' y' p'] m))
                seval/simplify
                seval/collapse-and-or))))

  (is (= '(and (= a' 200)
               (= b' 100))
         (let [m '{VARS- [x y a b]
                   b 100
                   a 200}]
           (->> '(CHANGED- [x y])
                (seval/seval* (seval/make-context '[x' y' a' b'] m))))))

  (is (= false
         (let [m {}]
           (->> '(comment "hello")
                (seval/seval* (seval/make-context  [] m))))))
  (is (= true
         (let [m {}]
           (->> '(= #{1 2} #{1 2})
                (seval/seval* (seval/make-context  [] m))))))

  (is (= '(or (let [x 1] (> 1 y'))
              (let [x 4] (> 4 y'))
              (let [x 3] (> 3 y'))
              (let [x 2] (> 2 y')))
         (let [m '{y 200}]
           (->> '(E [x (range* 1 4)]
                    (> x y'))
                (seval/seval* (seval/make-context '[y'] m))
                seval/simplify))))

  (is (= true
         (let [m {}]
           (->> '(= (union #{1 2} #{3}) #{1 2 3})
                (seval/seval* (seval/make-context [] m))))))

  (is (= true
         (let [m {}]
           (->> '(= (X #{1 2} #{3}) #{[2 3] [1 3]})
                (seval/seval* (seval/make-context [] m))))))
  (is (= true
         (let [m {}]
           (->> '(subset? #{1} #{1 2})
                (seval/seval* (seval/make-context [] m))))))
  (is (= true
         (let [m {}]
           (->> '(= (SUBSET #{1 2}) #{#{} #{2} #{1} #{1 2}})
                (seval/seval* (seval/make-context [] m))))))

  (is (= true
         (let [m {}]
           (->> '(= (select (fn [x] (> x 10)) #{15 5}) #{15})
                (seval/seval* (seval/make-context [] m))))))

  (is (= true
         (let [m {}]
           (->> '(= (map* (fn [x] (+ 1 x)) #{1 2}) #{3 2})
                (seval/seval* (seval/make-context [] m))))))

  (is (= true
         (let [m {}]
           (->> '(= (Cardinality #{3 2}) 2)
                (seval/seval* (seval/make-context [] m))))))

  (is (= '(CHOOSE [x #{1 4 3 2}]
                  (> y' x))
         (let [m '{y 200}]
           (->> '(CHOOSE [x (range* 1 4)]
                         (> y' x))
                (seval/seval* (seval/make-context '[y'] m))
                seval/simplify))))

  (is (= true
         (let [m {}]
           (->> '(= (first [10 20]) 10)
                (seval/seval* (seval/make-context [] m))))))

  (is (= true
         (let [m {}]
           (->> '(= (SelectSeq (fn [x] (> x 2)) [1 2 3 4]) [3 4])
                (seval/seval* (seval/make-context [] m))))))

  (is (= true
         (let [m {}]
           (->> '(= (DOMAIN [10 20]) #{1 2})
                (seval/seval* (seval/make-context [] m))))))

  (is (= true
         (let [m {}]
           (->> '(= (fm- [a #{20 10}] 30) {20 30, 10 30})
                (seval/seval* (seval/make-context [] m))))))

  (is (= true
         (let [m {}]
           (->> '(= (DOMAIN {:a 100, :b 200}) #{:b :a})
                (seval/seval* (seval/make-context [] m))))))

  (is (= true
         (let [m {}]
           (->> '(= (EXCEPT {:a 1, :b 2, :c 3} [:b] 20) {:a 1 :b 20 :c 3})
                (seval/seval* (seval/make-context [] m))))))

  (is (= [1 4]
         (let [v []
               m {}]
           (->> '(E [t #{1 2}]
                    (E [m #{3 4}]
                       [t m]))
                (seval/seval* (seval/make-context v m))
                (seval/seval* (seval/make-context  v m)))))))

(deftest test-3
  (is (= '(or (and (= badGuyStatus' "alive")
                   (= livingGoodGuys' #{:g4 :g2 :g1})
                   (= log' [["BadGuy defeats: " :g3]])
                   (= toDestroy' -1))
              (and (= badGuyStatus' "alive")
                   (= livingGoodGuys' #{:g4 :g2 :g3})
                   (= log' [["BadGuy defeats: " :g1]])
                   (= toDestroy' -1))
              (and (= badGuyStatus' "alive")
                   (= livingGoodGuys' #{:g4 :g3 :g1})
                   (= log' [["BadGuy defeats: " :g2]])
                   (= toDestroy' -1))
              (and (= badGuyStatus' "hasArtifact")
                   (= livingGoodGuys' #{:g2 :g3 :g1})
                   (= log' [["BadGuy defeats: " :g4] ["BadGuy has stone"]])
                   (= toDestroy' -1)))
         (let [v '[badGuyStatus'
                   livingGoodGuys'
                   log'
                   toDestroy']
               m (merge (seval/load-text (slurp "src/FinalMatch.clj"))
                        '{badGuyStatus "alive"
                          livingGoodGuys GoodGuys
                          log []
                          toDestroy -1}
                        '{GoodGuys #{:g1 :g2 :g3 :g4}
                          Strong #{:g3}
                          Flight #{:g2}
                          HasArtifact :g4})
               c (seval/make-context v m)]
           (->> '(Next)
                (seval/seval c))))))

(deftest test-4
  (is (= '(or (and (= msgs' #{{:type "Abort"}})
                   (= rmState' {:r1 "working", :r2 "working"})
                   (= tmPrepared' #{})
                   (= tmState' "aborted"))

              (and (= msgs' #{{:type "Prepared", :rm :r1}})
                   (= rmState' {:r1 "prepared", :r2 "working"})
                   (= tmPrepared' #{})
                   (= tmState' "init"))

              (and (= msgs' #{{:type "Prepared", :rm :r2}})
                   (= rmState' {:r1 "working", :r2 "prepared"})
                   (= tmPrepared' #{})
                   (= tmState' "init"))

              (and (= msgs' #{})
                   (= rmState' {:r1 "aborted", :r2 "working"})
                   (= tmPrepared' #{})
                   (= tmState' "init"))

              (and (= msgs' #{})
                   (= rmState' {:r1 "working", :r2 "aborted"})
                   (= tmPrepared' #{})
                   (= tmState' "init")))

         (let [v '[rmState'
                   tmState'
                   tmPrepared'
                   msgs']
               m (merge (seval/load-text (slurp "src/TwoPhase.clj"))
                        '{rmState {:r1 "working"
                                   :r2 "working"}
                          tmState "init"
                          tmPrepared #{}
                          msgs #{}}
                        '{RM #{:r1 :r2}}
                        '{VARS- [rmState
                                 tmState
                                 tmPrepared
                                 msgs]})
               c (seval/make-context v m)]
           (->> '(TPNext)
                (seval/seval c))))))

(deftest test-delta-state
  (is (= '{b' 21}
         (#'seval/delta-state-single '{a 10
                                       b 20}
                                     '{a' 10
                                       b' 21})))

  (is (= '#{{a' 11, b' 21}
            {b' 21}
            {}}
         (seval/delta-state '{a 10
                              b 20}
                            '#{{a' 10
                                b' 21}
                               {a' 11
                                b' 21}
                               {a' 10
                                b' 20}}))))

(deftest test-expand-CHOOSE
  (is (= :a2
         (seval/expand-CHOOSE (seval/make-context [] {}) '(CHOOSE [x #{:a1 :a2}] true))))
  (is (= 99
         (seval/expand-CHOOSE (seval/make-context [] {}) '(CHOOSE [x #{1 2 3 99 4 5 6}] (> x 50)))))
  (is (= ":_CHOOSE_FAIL/G_"
         (.substring (str (seval/expand-CHOOSE (seval/make-context [] {}) '(CHOOSE [x #{1 2 3 99 4 5 6}] (> x 500)))) 0 16))))

(deftest test-anon-f
  (let [anon-keyword (seval/define-anon-f
                       (seval/make-context '[a] '{b 100})
                       '(fn [x] (+ x 1)))]
    (is (= '{:body (fn [x] (+ x 1))
             :context {:free-v #{a}
                       :global-symbol-map {b 100}
                       :symbol-map-stack [{b 100}]
                       :super-stack []}}
           (eval `(seval/resolve-anon-f ~anon-keyword))))))

(deftest test-CHANGED
  (is (= '(and (= a' a) (= b' b)))
      (seval/eval-UNCHANGED '[a b])) (is (= '(UNCHANGED [a b])
                                            (seval/eval-CHANGED- (seval/make-context '[x' y' a' b']
                                                                                     '{VARS- [x y a b]})
                                                                 '[x y]))))

(deftest test-eval-cond
  (is (= '(or (and (> x 2)
                   200)
              (and (> x 1)
                   (not (> x 2))
                   100)
              (and true
                   (not (> x 2))
                   (not (> x 1))
                   99)))
      (#'seval/eval-cond '[(> x 2) 200
                           (> x 1) 100
                           :default 99])))

(deftest test-context
  (is (= 20
         (-> (seval/make-context [])
             (seval/push-context {'a 1
                                  'b 2})
             (seval/push-context {'m 10
                                  'p 20})
             (seval/push-context {'x 100
                                  'y 200})
             (seval/push-context {'z1 1000
                                  'z2 2000})
             (seval/resolve-in-context 'p)))))

;; (time (run-tests))
