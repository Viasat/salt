(ns salt.test-simplify
  (:require [clojure.test :refer :all]
            [salt]
            [salt.lang :refer :all]
            [salt.simplify :as simplify]
            [tlaplus.FiniteSets :refer :all]
            [tlaplus.Integers :refer :all]
            [tlaplus.Sequences :refer :all]))

(use-fixtures :each (salt/namespace-fixture 'salt.test-simplify))

(deftest test-0
  (is (= [false false false false true true]
         [(#'simplify/expr-with-free-vars? (simplify/make-context '[a']) 100)
          (#'simplify/expr-with-free-vars? (simplify/make-context '[a']) 'a)
          (#'simplify/expr-with-free-vars? (simplify/make-context '[a']) [])
          (#'simplify/expr-with-free-vars? (simplify/make-context '[a']) '(+ a [:d {:a [1 2 #{"r" a}]}]))
          (#'simplify/expr-with-free-vars? (simplify/make-context '[a']) 'a')
          (#'simplify/expr-with-free-vars? (simplify/make-context '[a']) '(+ a [:d {:a [1 2 #{"r" a'}]}]))]))

  (is (= '(or (let [x 1] (and false (= x' 1)))
              (let [x 3] (and true (= x' 3)))
              (let [x 2] (and true (= x' 2))))
         (#'simplify/expand-E (simplify/make-context ['x'] {})
                              '(E [x #{1 3 2}] (and (>= x 2) (= x' x)))))))

(deftest test-1
  (is (= '(or false (> q' 20))
         (let [m {'x 1
                  'a 20}]
           (->> '(or (let [a 9]
                       (> x a))
                     (> q' a))
                (simplify/seval* (simplify/make-context '[q'] m))))))

  (is (= '(and z'
               (<= q' (+ t' 300)))
         (let [c (simplify/make-context '[z' t' q']
                                        '{a true
                                          b false})]
           (->> '(and a
                      (or b z')
                      (or b
                          (<= q' (+ 100 200 t'))))
                (simplify/seval* c)
                (simplify/apply-rules c)))))

  (is (= '(or (let [x 1] false)
              (let [x 3] true)
              (let [x 2] true))
         (let [c (simplify/make-context [] '{s #{1 2 3}})]
           (->> '(E [x s]
                    (>= x 2))
                (simplify/seval* c)
                (simplify/apply-rules c)))))

  (is (= '(or (and (= x' 2)
                   (= y' 203))
              (and (= x' 3)
                   (= y' 204)))
         (let [m '{a true
                   b false
                   x 100
                   y 200
                   s #{1 2 3}}]
           (let [c (simplify/make-context '[x' y' c'] m)]
             (->> '(and (E [x s]
                           (and (>= x 2)
                                (= x' x)
                                (and (= y' (+ 1 y x')))))
                        (or a
                            b
                            (or c'
                                (> x 9999))))
                  (simplify/seval c))))))

  (is (= '(and (< x' 201)
               (< x' 203)
               (< x' 202))
         (let [m '{s #{1 2 3}
                   x 100}
               c (simplify/make-context '[x'] m)]
           (->> '(A [x s]
                    (and (> x 0)
                         (>= (+ x 200) x')))
                (simplify/seval* c)
                (simplify/apply-rules c)))))

  (is (= '[true
           z'
           (or z'
               (or q' p'))]
         [(simplify/apply-rules (simplify/make-context [] {}) '(or false z' true))
          (simplify/apply-rules (simplify/make-context [] {}) '(and true z'))
          (simplify/apply-rules (simplify/make-context [] {}) '(or (or false z') (or q' p')))]))

  (is (= false
         (let [m '{foo (fn [r]
                         r)}]
           (->> '(foo false)
                (simplify/seval* (simplify/make-context  [] m))))))

  (is (= '(or (<= z' 50)
              (= x' 99)
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
                         (and p (> x (+ r z'))))}
               c (simplify/make-context '[x' t' z'] m)]
           (->> '(or (= 200 (+ 1 x x'))
                     (foo 10))
                (simplify/seval c)))))

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
                (simplify/seval* (simplify/make-context [] m))))))

  (is (= 3
         (let [m '{Add (fn [x y]
                         (+ x y))
                   ApplyF (fn [f x y]
                            (f x y))
                   CallF (fn []
                           (ApplyF Add 1 2))}]
           (->> '(CallF)
                (simplify/seval* (simplify/make-context [] m))))))

  (is (= 21
         (let [m '{ApplyF (fn [f x y]
                            (f x y))
                   CallF (fn []
                           (ApplyF (fn [a b]
                                     (+ a b))
                                   1
                                   20))}]
           (->> '(CallF)
                (simplify/seval* (simplify/make-context [] m))))))

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
                (simplify/seval* (simplify/make-context [] m))))))
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
                (simplify/seval* (simplify/make-context [] m))))))

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
                (simplify/seval* (simplify/make-context [] m))))))

  (is (= 'q'
         (let [m {'x 1}
               c (simplify/make-context '[p' q'] m)]
           (->> '(if (> x 5)
                   p'
                   q')
                (simplify/seval c)))))

  (is (nil?
       (let [m {'x 1}
             c (simplify/make-context '[p' q'] m)]
         (->> '(cond (> x 5) p'
                     (> x 10) q')
              (simplify/seval c)))))

  (is (= 'p'
         (let [m {'x 6}
               c (simplify/make-context '[p' q'] m)]
           (->> '(cond (> x 5) p'
                       (> x 10) q')
                (simplify/seval c)))))

  (is (= '(cond true p'
                true q')
         (let [m {'x 16}
               c (simplify/make-context '[p' q'] m)]
           (->> '(cond (> x 5) p'
                       (> x 10) q')
                (simplify/seval c)))))

  (is (= 'p'
         (let [m {'x 16}
               c (simplify/make-context '[p' q'] m)]
           (->> '(cond (> x 5) p'
                       (> x 10) p')
                (simplify/seval c)))))

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
                (simplify/seval* (simplify/make-context '[q'] m))))))

  (is (= '(or (let [x 0] false)
              (let [x 1] false)
              (let [x 3] (> x' 3))
              (let [x 2] (> x' 2)))
         (let [m {}
               c (simplify/make-context '[x'] m)]
           (->> '(E [x (Nat)]
                    (and (> x 1)
                         (> x' x)))
                (simplify/seval* c)
                (simplify/apply-rules c)))))

  (is (= '(and (=> true x')
               (<=> true x'))
         (let [m {'x true}]
           (->> '(and (=> x x')
                      (<=> x x'))
                (simplify/seval* (simplify/make-context '[x'] m))))))

  (is (= '(and false
               (not= 100 x'))
         (let [m {'x 100}]
           (->> '(and (not= (> x 5))
                      (not= x x'))
                (simplify/seval* (simplify/make-context '[x'] m))))))

  (is (= '(and (= p' 20)
               (= x' 100)
               (= y' 200))
         (let [m {'x 100
                  'y 200}
               c (simplify/make-context '[x' y' p'] m)]
           (->> '(and
                  (= p' 20)
                  (UNCHANGED [x y]))
                (simplify/seval* c)
                (simplify/apply-rules c)
                simplify/collapse-and-or))))

  (is (= '(and (= a' 200)
               (= b' 100))
         (let [m '{VARS- [x y a b]
                   b 100
                   a 200}]
           (->> '(CHANGED- [x y])
                (simplify/seval* (simplify/make-context '[x' y' a' b'] m))))))

  (is (= false
         (let [m {}]
           (->> '(comment "hello")
                (simplify/seval* (simplify/make-context  [] m))))))
  (is (= true
         (let [m {}]
           (->> '(= #{1 2} #{1 2})
                (simplify/seval* (simplify/make-context  [] m))))))

  (is (= '(or (let [x 1] (<= y' 1))
              (let [x 4] (<= y' 4))
              (let [x 3] (<= y' 3))
              (let [x 2] (<= y' 2)))
         (let [m '{y 200}
               c (simplify/make-context '[y'] m)]
           (->> '(E [x (range* 1 4)]
                    (> x y'))
                (simplify/seval* c)
                (simplify/apply-rules c)))))

  (is (= true
         (let [m {}]
           (->> '(= (union #{1 2} #{3}) #{1 2 3})
                (simplify/seval* (simplify/make-context [] m))))))

  (is (= true
         (let [m {}]
           (->> '(= (X #{1 2} #{3}) #{[2 3] [1 3]})
                (simplify/seval* (simplify/make-context [] m))))))
  (is (= true
         (let [m {}]
           (->> '(subset? #{1} #{1 2})
                (simplify/seval* (simplify/make-context [] m))))))
  (is (= true
         (let [m {}]
           (->> '(= (SUBSET #{1 2}) #{#{} #{2} #{1} #{1 2}})
                (simplify/seval* (simplify/make-context [] m))))))

  (is (= true
         (let [m {}]
           (->> '(= (select (fn [x] (> x 10)) #{15 5}) #{15})
                (simplify/seval* (simplify/make-context [] m))))))

  (is (= true
         (let [m {}]
           (->> '(= (map* (fn [x] (+ 1 x)) #{1 2}) #{3 2})
                (simplify/seval* (simplify/make-context [] m))))))

  (is (= true
         (let [m {}]
           (->> '(= (Cardinality #{3 2}) 2)
                (simplify/seval* (simplify/make-context [] m))))))

  (is (= '(CHOOSE [x #{1 4 3 2}]
                  (> y' x))
         (let [m '{y 200}
               c (simplify/make-context '[y'] m)]
           (->> '(CHOOSE [x (range* 1 4)]
                         (> y' x))
                (simplify/seval* c)
                (simplify/apply-rules c)))))

  (is (= true
         (let [m {}]
           (->> '(= (first [10 20]) 10)
                (simplify/seval* (simplify/make-context [] m))))))

  (is (= true
         (let [m {}]
           (->> '(= (SelectSeq (fn [x] (> x 2)) [1 2 3 4]) [3 4])
                (simplify/seval* (simplify/make-context [] m))))))

  (is (= true
         (let [m {}]
           (->> '(= (DOMAIN [10 20]) #{1 2})
                (simplify/seval* (simplify/make-context [] m))))))

  (is (= true
         (let [m {}]
           (->> '(= (fm- [a #{20 10}] 30) {20 30, 10 30})
                (simplify/seval* (simplify/make-context [] m))))))

  (is (= true
         (let [m {}]
           (->> '(= (DOMAIN {:a 100, :b 200}) #{:b :a})
                (simplify/seval* (simplify/make-context [] m))))))

  (is (= true
         (let [m {}]
           (->> '(= (EXCEPT {:a 1, :b 2, :c 3} [:b] 20) {:a 1 :b 20 :c 3})
                (simplify/seval* (simplify/make-context [] m))))))

  (is (= [1 4]
         (let [v []
               m {}]
           (->> '(E [t #{1 2}]
                    (E [m #{3 4}]
                       [t m]))
                (#'simplify/seval** (simplify/make-context v m)))))))

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
               m (merge (simplify/load-text (slurp "test/FinalMatch.clj"))
                        '{badGuyStatus "alive"
                          livingGoodGuys GoodGuys
                          log []
                          toDestroy -1}
                        '{GoodGuys #{:g1 :g2 :g3 :g4}
                          Strong #{:g3}
                          Flight #{:g2}
                          HasArtifact :g4})
               c (simplify/make-context v m)]
           (->> '(Next)
                (simplify/seval c))))))

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
               m (merge (simplify/load-text (slurp "test/TwoPhase.clj"))
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
               c (simplify/make-context v m)]
           (->> '(TPNext)
                (simplify/seval c))))))

(deftest test-delta-state
  (is (= '{b' 21}
         (#'simplify/delta-state-single '{a 10
                                          b 20}
                                        '{a' 10
                                          b' 21})))

  (is (= '#{{a' 11, b' 21}
            {b' 21}
            {}}
         (simplify/delta-state '{a 10
                                 b 20}
                               '#{{a' 10
                                   b' 21}
                                  {a' 11
                                   b' 21}
                                  {a' 10
                                   b' 20}})))

  (is (= '{a' 11}
         (#'simplify/delta-state-single '{a 10
                                          b' 20}
                                        '{a' 11
                                          b 20})))

  (is (= '(+ 1 x)
         (#'simplify/to-state [] '(+ 1 x))))

  (is (= '(and (+ 1 x)
               (= x' 100))
         (#'simplify/to-state [] '(and (+ 1 x)
                                       (= x' 100)))))

  (is (= '(and (+ 1 x)
               (= x' 100))
         (#'simplify/delta-state '{x 2} '(and (+ 1 x)
                                              (= x' 100)))))

  (is (= '{x' 100}
         (->> '(and (= y' 3)
                    (= x' 100))
              (#'simplify/to-state '[x' y'])
              (#'simplify/delta-state '{x 2
                                        y 3}))))

  (is (= '#{{y' 300}
            {x' 100}}
         (->> '(or (and (= y' 3)
                        (= x' 100))
                   (and (= y' 300)
                        (= x' 2)))
              (#'simplify/to-state '[x' y'])
              (#'simplify/delta-state '{x 2
                                        y 3})))))

(deftest test-expand-CHOOSE
  (is (= :a2
         (simplify/expand-CHOOSE (simplify/make-context [] {}) '(CHOOSE [x #{:a1 :a2}] true))))
  (is (= 99
         (simplify/expand-CHOOSE (simplify/make-context [] {}) '(CHOOSE [x #{1 2 3 99 4 5 6}] (> x 50)))))
  (is (= ":_CHOOSE_FAIL/G_"
         (.substring (str (simplify/expand-CHOOSE (simplify/make-context [] {}) '(CHOOSE [x #{1 2 3 99 4 5 6}] (> x 500)))) 0 16))))

(deftest test-anon-f
  (let [anon-keyword (simplify/define-anon-f
                       (simplify/make-context '[a] '{b 100})
                       '(fn [x] (+ x 1)))]
    (is (= '{:body (fn [x] (+ x 1))
             :context {:free-v #{a}
                       :global-symbol-map {b 100}
                       :symbol-map-stack [{b 100}]
                       :super-stack []}}
           (eval `(simplify/resolve-anon-f ~anon-keyword))))))

(deftest test-CHANGED
  (is (= '(and (= a' a) (= b' b))
         (simplify/eval-UNCHANGED '[a b]))) (is (= '(UNCHANGED [a b])
                                                   (simplify/eval-CHANGED- (simplify/make-context '[x' y' a' b']
                                                                                                  '{VARS- [x y a b]})
                                                                           '[x y]))))

(deftest test-eval-cond
  (is (= '(cond (> x 2) 200
                (> x 1) 100
                (and (not (> x 2))
                     (not (> x 1))) 99)
         (#'simplify/eval-cond (simplify/make-context '[x] {})
                               '[(> x 2) 200
                                 (> x 1) 100
                                 :default 99]))))

(deftest test-context
  (is (= 20
         (-> (simplify/make-context [])
             (simplify/push-context {'a 1
                                     'b 2})
             (simplify/push-context {'m 10
                                     'p 20})
             (simplify/push-context {'x 100
                                     'y 200})
             (simplify/push-context {'z1 1000
                                     'z2 2000})
             (simplify/resolve-in-context 'p)))))

(deftest test-simplify
  (is (= '(and (< c3 -6)
               (< y3 -5)
               (<= c2 28)
               (<= z3 4)
               (= (expt z2 2) 15)
               (= b false)
               (= c 28)
               (= x 2)
               (= y 5)
               (= z 4)
               (> y2 5)
               (>= a -20))
         (let [m {}
               c (simplify/make-context '[x y y2 y3 z z2 z3 a b c c2 c3] m)]
           (->> '(and (= (- 25 x) (+ 20 3))
                      (= (* 20 y) 100)
                      (> (* 20 y2) 100)
                      (< (* -2 y3) 10)
                      (= (expt z 2) 16)
                      (= (expt z2 2) 15)
                      (<= (expt z3 2) 16)
                      (< (- a) 20)
                      (= (not b) true)
                      (= (div c 4) 7)
                      (<= (div c2 4) 7)
                      (> (div c3 -2) 3))
                (simplify/seval c)))))

  (is (= '(or (= x #{1 4 2})
              (= x #{1 4 3 2})
              (= x #{1 4 3})
              (= x #{1 4})

              (= y #{1 3 2})
              (= y #{1 3})
              (= y #{1 4 3 2})
              (= y #{1 4 3})

              (subset? #{2} z))
         (let [m {}
               c (simplify/make-context '[x y y2 y3 z a b c c2 c3] m)]
           (->> '(or (= #{1 2 3 4} (union #{2 3} x))
                     (= #{1 3} (difference y #{2 4}))
                     (= #{1 3} (difference #{1 2 3} z))
                     (= #{4} (difference #{1 2 3} a)))
                (simplify/seval c)))))

  (is (not
       (let [m {}
             c (simplify/make-context '[x] m)]
         (->> '(= #{1 2 4} (union #{2 3} x)) ;; impossible union
              (simplify/seval c)))))

  (is (= '(= x [30 40])
         (let [m {}
               c (simplify/make-context '[x])]
           (simplify/seval c '(= [10 20 30 40] (into [10 20] x))))))
  (is (= false
         (let [m {}
               c (simplify/make-context '[x])]
           (simplify/seval c '(= [10 15 20 30 40] (into [10 20] x))))))
  (is (= '(= (into [10 20] x) #{1 2})
         (let [m {}
               c (simplify/make-context '[x])]
           (simplify/seval c '(= #{1 2} (into [10 20] x))))))

  (is (= false
         (let [m {}
               c (simplify/make-context '[x])]
           (simplify/seval c '(= [10 20 30 40] (into x [10 20]))))))

  (is (= '(= x [10 20])
         (let [m {}
               c (simplify/make-context '[x])]
           (simplify/seval c '(= [10 20 30 40] (into x [30 40])))))))

(deftest test-simply-EXCEPT
  (is (not
       (let [m {}
             c (simplify/make-context '[a] m)]
         (->> '(= (EXCEPT a [:f] "done") {:f "started"})
              (simplify/seval c)))))

  (is (= '(not= (EXCEPT a [:f] "done") {:f "started"})
         (let [m {}
               c (simplify/make-context '[a] m)]
           (->> '(not= (EXCEPT a [:f] "done") {:f "started"})
                (simplify/seval c)))))

  (is (= '(= (EXCEPT a [:f] "done") {:f "done"})
         (let [m {}
               c (simplify/make-context '[a] m)]
           (->> '(= (EXCEPT a [:f] "done") {:f "done"})
                (simplify/seval c)))))

  (is (not
       (let [m {}
             c (simplify/make-context '[a] m)]
         (->> '(not= (EXCEPT a [:f] "done") {:f "done"})
              (simplify/seval c))))))

(deftest test-simply-EXCEPT-combine-clauses
  (is (= '(= a {:f "init"})
         (let [m {}
               c (simplify/make-context '[a] m)]
           (->> '(and (= (EXCEPT a [:f] "done") {:f "done"})
                      (= (get* a :f) "init"))
                (simplify/seval c)))))

  (is (not
       (let [m {}
             c (simplify/make-context '[a] m)]
         (->> '(and (= (EXCEPT a [:f] "done") {:f "not-done"})
                    (= (get* a :f) "init"))
              (simplify/seval c)))))

  (is (= '(and (= (EXCEPT a [:f] "done") {:f "done"})
               (= (get* a :g) "init"))
         (let [m {}
               c (simplify/make-context '[a] m)]
           (->> '(and (= (EXCEPT a [:f] "done") {:f "done"})
                      (= (get* a :g) "init"))
                (simplify/seval c))))))

(defn foo [x]
  (when (> x 10)
    (inc x)))

(defn bar [x]
  (when (> x 5)
    (dec x)))

(defn baz [x]
  (when (> x 2)
    (* x 2)))

(deftest test-rules->>
  (is (= 21
         (simplify/rule->> 20
                           foo
                           bar
                           baz)))

  (is (= 9
         (simplify/rule->> 10
                           foo
                           bar
                           baz)))

  (is (= 10
         (simplify/rule->> 5
                           foo
                           bar
                           baz)))

  (is (= 0
         (simplify/rule->> 0
                           foo
                           bar
                           baz))))

(deftest test-simulate
  (is (= '#{{y' 201
             x' 2}
            {y' 201
             x' 3}}
         (salt/with-rand-seed 20
           (->> '(and (E [x s]
                         (and (>= x 2)
                              (= x' x)
                              (and (= y' (+ 1 y)))))
                      (or a
                          b
                          (or c'
                              (> x 9999))))
                (simplify/simulate** '[x' y' c'] '{a true
                                                   b false
                                                   s #{1 2 3}}
                                     '{x 100
                                       y 200}
                                     10))))))

;; (time (run-tests))
