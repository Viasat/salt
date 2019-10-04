(ns salt.simplify
  (:require [clojure.set :as set]
            [salt.lang :refer :all]
            [tlaplus.FiniteSets :refer :all]
            [tlaplus.Integers :refer :all]
            [tlaplus.Sequences :refer :all])
  (:import [java.util Random]))

(defn listy? [x]
  (or (list? x)
      (and (seq? x)
           (not (vector? x))
           (not (map? x))
           (not (set? x)))))

;;

(defn valid-context? [c]
  (when-not (and (map? c)
                 (= #{:free-v :symbol-map-stack :global-symbol-map :super-stack :simulate?}) (keys c))
    (throw (RuntimeException. (str "invalid context " c)))))

(defn produce-context [c]
  (valid-context? c)
  c)

(defn make-context
  ([free-v]
   (produce-context {:free-v (set free-v)
                     :symbol-map-stack []}))
  ([free-v m]
   (when-not (map? m)
     (throw (RuntimeException. (str "cannot make context with: " m))))
   (produce-context {:free-v (set free-v)
                     :global-symbol-map m
                     :symbol-map-stack [m]
                     :super-stack []})))

(defn debug-context [c]
  (rest (:symbol-map-stack c)))

(def ^:dynamic *simulated-vars* (atom {}))

(defn allow-simulation [c]
  (reset! *simulated-vars* {})
  (produce-context (assoc c :simulate? true)))

(defn clear-simulation [c]
  (valid-context? c)
  (reset! *simulated-vars* {})
  (produce-context (dissoc c :simulate?)))

(defn should-simulate? [c]
  (valid-context? c)
  (:simulate? c))

(defn simulate [c var val]
  (swap! *simulated-vars* #(assoc % var val)))

(defn push-context [c m]
  (valid-context? c)
  (when-not (map? m)
    (throw (RuntimeException. (str "cannot push to context: " m))))
  (produce-context (assoc-in c [:symbol-map-stack] (conj (:symbol-map-stack c) m))))

(defn super-push-context [c m]
  (valid-context? c)
  (produce-context (-> c
                       (assoc-in [:super-stack] (conj (:super-stack c (:symbol-map-stack c))))
                       (assoc-in [:symbol-map-stack] [(:global-symbol-map c) m]))))

(defn- is-fn? [e]
  (and (listy? e)
       (= 'fn (first e))))

(defn resolve-in-context
  ([c s]
   (resolve-in-context c s false))
  ([c s peek?]
   (valid-context? c)
   (let [{:keys [symbol-map-stack]} c]
     (when-not (vector? symbol-map-stack)
       (throw (RuntimeException. (str "invalid context: " [s c]))))
     (loop [cs symbol-map-stack]
       (let [c0 (last cs)]
         (if c0
           (let [r (let [c0-r (c0 s)]
                     (if (nil? c0-r)
                       (when (should-simulate? c)
                         (@*simulated-vars* s))
                       c0-r))
                 r (if (or peek?
                           (not (is-fn? r)))
                     r
                     s)]
             (if (nil? r)
               (recur (vec (butlast cs)))
               r))
           (when-not peek?
             (throw (RuntimeException. (str "symbol not found: " [s (rest c)]))))))))))

(defn is-free-v? [c s]
  (valid-context? c)
  (and (nil? (resolve-in-context c s true))
       (boolean ((:free-v c) s))))

;;

(declare expr-with-free-vars?)

(defn- collection-with-free-vars? [context e]
  (->> e
       (map (partial expr-with-free-vars? context))
       (some identity)))

(defn- map-with-free-vars? [context e]
  (or (collection-with-free-vars? context (keys e))
      (collection-with-free-vars? context (vals e))))

(defn- expr-with-free-vars? [context e]
  (boolean
   (cond
     (listy? e) (collection-with-free-vars? context e)
     (symbol? e) (is-free-v? context e)
     (set? e) (collection-with-free-vars? context e)
     (map? e) (map-with-free-vars? context e)
     (vector? e) (collection-with-free-vars? context e)
     (string? e) false
     (number? e) false
     (keyword? e) false
     (boolean? e) false)))

(declare expr-with-symbol-references?)

(defn- collection-with-symbol-references? [context e]
  (->> e
       (map (partial expr-with-symbol-references? context))
       (some identity)))

(defn- map-with-symbol-references? [context e]
  (or (collection-with-symbol-references? context (keys e))
      (collection-with-symbol-references? context (vals e))))

(defn- expr-with-symbol-references? [context e]
  (boolean
   (cond
     (listy? e) (collection-with-symbol-references? context e)
     (symbol? e) true
     (set? e) (collection-with-symbol-references? context e)
     (map? e) (map-with-symbol-references? context e)
     (vector? e) (collection-with-symbol-references? context e)
     (string? e) false
     (number? e) false
     (keyword? e) false
     (boolean? e) false)))

;;;;

(declare expr-with-unbound-symbol-references?)

(defn- collection-with-unbound-symbol-references? [context e]
  (->> e
       (map (partial expr-with-unbound-symbol-references? context))
       (some identity)))

(defn- map-with-unbound-symbol-references? [context e]
  (or (collection-with-unbound-symbol-references? context (keys e))
      (collection-with-unbound-symbol-references? context (vals e))))

(defn- expr-with-unbound-symbol-references? [context e]
  (boolean
   (cond
     (listy? e) (collection-with-unbound-symbol-references? context e)
     (symbol? e) (let [result (resolve-in-context context e true)]
                   (or (nil? result)
                       (= result e)))
     (set? e) (collection-with-unbound-symbol-references? context e)
     (map? e) (map-with-unbound-symbol-references? context e)
     (vector? e) (collection-with-unbound-symbol-references? context e)
     (string? e) false
     (number? e) false
     (keyword? e) false
     (boolean? e) false)))

;;

(defn- process-map [f e]
  (zipmap (map f (keys e))
          (map f (vals e))))

(declare seval**)

(defn- eval-E-A [context e]
  (let [op (first e)]
    (let [[_ bindings body] e]
      (if (expr-with-free-vars? context bindings)
        (list op bindings body)
        (let [[v s] bindings]
          (list op bindings body))))))

(defn- eval-cond [context clauses]
  (let [conds (vec (->> clauses
                        (partition 2)
                        (map first)
                        (map (partial seval** context))))
        bodies (vec (map second (partition 2 clauses)))
        options (mapcat (fn [b n]
                          (let [c (get conds n)
                                b (get bodies n)
                                m (map (fn [x] `(~'not ~x)) (take n conds))
                                c (if (= :default c)
                                    `(~'and ~@m)
                                    c)]
                            (when-not (false? c)
                              `(~c ~b))))
                        bodies
                        (range))]
    (if (and (every? true? conds)
             (= 1 (count (set bodies))))
      (first bodies)
      (when-not (every? false? conds)
        `(~'cond ~@(remove nil? options))))))

(defn primed-symbol [s]
  (when-not (symbol? s)
    (throw (RuntimeException. (str "Expected a symbol instead of: " [s (class s)]))))
  (symbol (str (name s) "'")))

(defn eval-UNCHANGED [vs]
  `(~'and ~@(->> vs
                 (map (fn [us]
                        (let [s (primed-symbol us)]
                          `(~'= ~s ~us)))))))

(defn eval-CHANGED- [context vs]
  (let [vs (vec (set/difference (set (resolve-in-context context 'VARS-)) (set vs)))]
    `(~'UNCHANGED ~vs)))

(declare expand-fm-)

(def ^:dynamic *anon-f* (atom {}))

(defn new-anon-f-s []
  (keyword "anon-f" (str (gensym))))

(defn define-anon-f [context e]
  (let [s (keyword "anon-f" (str (gensym)))]
    (swap! *anon-f* #(assoc % s {:body e
                                 :context context}))
    s))

(defn resolve-anon-f [op]
  (get @*anon-f* op))

(defn debug-anon-f []
  (let [anon-f @*anon-f*]
    (zipmap (keys anon-f)
            (map (comp debug-context :context) (vals anon-f)))))

(defn- resolve-custom-f [context op]
  (let [result (let [f (fn [op]
                         (if (keyword? op)
                           (let [{:keys [body context]} (resolve-anon-f op)]
                             [context body])
                           [(super-push-context context {}) (resolve-in-context context op true)]))]
                 (loop [last [context op]
                        current (f op)
                        n 20]
                   (let [[_ r] current]
                     (if (or (= last current)
                             (is-fn? r)
                             (zero? n))
                       current
                       (recur current (f r) (dec n))))))]
    result))

(defn expand-E [context e]
  (let [[_ [v s] & body] e
        body (last body)]
    (if (expr-with-free-vars? context s)
      e
      (do
        (when (symbol? s)
          (throw (RuntimeException. (str "expand-E s= " s))))
        (apply list (conj (for [x s]
                            `(~'let [~v ~x]
                                    ~(seval** (push-context context {v x})
                                              body)))
                          'or))))))

(defn expand-A [context e]
  (let [[_ [v s] & body] e
        body (last body)]
    (if (expr-with-free-vars? context s)
      e
      (apply list (conj (for [x s]
                          (seval** (push-context context {v x}) body))
                        'and)))))

(defn expand-CHOOSE [context e]
  (let [[_ [v s] & body] e
        body (last body)]
    (if (or (expr-with-free-vars? context s)
            (expr-with-free-vars? context body))
      e
      (or (->> s
               (map (fn [x]
                      (when (true? (seval** (push-context context {v x})
                                            body))
                        x)))
               (remove nil?)
               first)
          (keyword "_CHOOSE_FAIL" (str (gensym)))))))

(defn- expand-E-A [context e]
  (cond
    (listy? e) (condp = (first e)
                 'E (expand-E context e)
                 'A (expand-A context e)
                 'CHOOSE (expand-CHOOSE context e)
                 (apply list (first e) (map (partial expand-E-A context) (rest e))))
    (symbol? e) e
    (set? e) (set (map (partial expand-E-A context) e))
    (map? e) (zipmap (map (partial expand-E-A context) (keys e))
                     (map (partial expand-E-A context) (vals e)))
    (vector? e) (vec (map (partial expand-E-A context) e))
    (string? e) e
    (number? e) e
    (keyword? e) e
    (boolean? e) e))

(defn- expand-fm- [context e]
  (let [[_ [v s] body] e]
    (if (expr-with-free-vars? context s)
      e
      (reduce merge {} (for [x s]
                         {x (seval** (push-context context {v x}) body)})))))

(defn- scalar? [x]
  (or (map? x)
      (vector? x)
      (set? x)
      (number? x)
      (string? x)))

;;

(def ^:dynamic *rand-seed-atom* (atom (rand-int Integer/MAX_VALUE)))

(defn rand-int* [n]
  (let [r (Random. @*rand-seed-atom*)
        result (.nextInt r n)
        new-seed (.nextInt r Integer/MAX_VALUE)]
    (reset! *rand-seed-atom* new-seed)
    result))

;;

(defn- optionally-simulate [context e]
  (let [[op x y] e]
    (when (and (= op '=)
               (symbol? x)
               (expr-with-free-vars? context x)
               (not (expr-with-free-vars? context y))
               (scalar? y))
      (when (= 1 (rand-int* 2))
        (simulate context x y)))))

(defn seval* [context e]
  (try (if (= '() e)
         []
         (cond
           (is-fn? e) (define-anon-f context e)

           (listy? e) (let [op (first e)]
                        (cond
                          (= 'line- op)
                          false

                          (#{'E 'A 'CHOOSE} op)
                          (let [[op [v s] & body] e
                                evald (eval-E-A context (list op [v (seval** context s)]
                                                              (last body)))
                                expanded (expand-E-A context evald)]
                            expanded)

                          (#{'= 'not=
                             '> '>= '< '<= '=> '<=>
                             '+ '- '* 'div 'mod* 'expt
                             'and 'or 'not
                             'Nat 'range*
                             'union 'UNION 'X 'subset? 'SUBSET 'contains? 'Cardinality 'difference 'intersection
                             'first 'get* 'rest* 'into 'count
                             'SubSeq 'conj 'Seq 'every?* 'DOMAIN
                             'maps-} op)
                          (let [[_ & args] e]
                            (if (and (zero? (count args))
                                     (#{'and 'or} op))
                              (condp = op
                                'and true
                                'or false)
                              (let [evaled-args (vec (map (partial seval** context) args))
                                    result (apply list op evaled-args)]
                                (if (expr-with-symbol-references? context evaled-args)
                                  (do
                                    (optionally-simulate context result)
                                    result)
                                  (eval result)))))

                          (= 'EXCEPT op)
                          (let [[_ old path new] e
                                [_ & args] e]
                            (if (and (listy? new)
                                     (#{'fn* 'fn} (first new)))
                              (let [[_ [f-arg] & f-body] new
                                    f-body (seval** (push-context context {f-arg f-arg}) (last f-body))
                                    old (seval** context old)
                                    path (seval** context path)
                                    result `(EXCEPT ~old ~path (~'fn ~(if f-arg
                                                                        [f-arg]
                                                                        []) ~f-body))]
                                (if (or (expr-with-free-vars? context f-body)
                                        (expr-with-free-vars? context old)
                                        (expr-with-free-vars? context path))
                                  result
                                  (eval result)))
                              (let [evaled-args (vec (map (partial seval** context) args))
                                    result (apply list op evaled-args)]
                                (if (expr-with-symbol-references? context evaled-args)
                                  result
                                  (eval result)))))

                          (= 'let op)
                          (let [[_ bindings & body] e]
                            (seval** (push-context context (zipmap (map first (partition 2 bindings))
                                                                   (map second (partition 2 bindings))))
                                     (last body)))

                          (= 'fm- op)
                          (let [[_ bindings & body] e
                                body (seval** (push-context context
                                                            (zipmap (map first (partition 2 bindings))
                                                                    (map second (partition 2 bindings))))
                                              (last body))]
                            (expand-fm- context (list op bindings body)))

                          (= 'if op)
                          (let [[_ clause a b] e]
                            (let [clause-result (seval** context clause)]
                              (cond
                                (true? clause-result)
                                (seval** context `~a)
                                (false? clause-result)
                                (seval** context `~b)
                                :default
                                (seval** context `(~'or (~'and ~clause
                                                               ~a)
                                                        (~'and (~'not ~clause)
                                                               ~b))))))

                          (= 'cond op)
                          (let [[_ & clauses] e]
                            (eval-cond context clauses))

                          (= 'UNCHANGED op)
                          (let [[_ vs] e]
                            (seval** context (eval-UNCHANGED
                                              (if (vector? vs)
                                                vs
                                                (seval** context vs)))))

                          (= 'CHANGED- op)
                          (let [[_ vs] e]
                            (seval** context (eval-CHANGED- context vs)))

                          (#{'select 'SelectSeq} op)
                          (let [[op [_ [f-arg] & f-body] s] e
                                f-body (seval** (push-context context {f-arg f-arg}) (last f-body))
                                s (seval** context s)
                                result `(~op (~'fn [~f-arg] ~f-body) ~s)]
                            (if (or (expr-with-free-vars? context f-body)
                                    (expr-with-free-vars? context s))
                              result
                              (eval result)))

                          (= 'map* op)
                          (let [[op [_ [f-arg] & f-body] s] e
                                f-body (seval** (push-context context {f-arg f-arg}) (last f-body))
                                s (seval** context s)
                                result `(~op (~'fn [~f-arg] ~f-body) ~s)]
                            (if (or (expr-with-free-vars? context f-body)
                                    (expr-with-free-vars? context s))
                              result
                              (if (not (expr-with-free-vars? context s))
                                (map* #(seval** (push-context context {f-arg %}) f-body) s)
                                (eval result))))

                          (= 'comment op)
                          false

                          :default
                          (if (nil? op)
                            (throw (RuntimeException. (str "Cannot resolve nil: " [e (class e)])))
                            (let [args (vec (map (partial seval** context) (rest e)))
                                  result (apply list (first e) args)]
                              (if (expr-with-unbound-symbol-references? context args)
                                result
                                (let [[sub-context f-form] (resolve-custom-f context op)]
                                  (when (nil? f-form)
                                    (throw (RuntimeException. (str "cannot resolve op: " [op
                                                                                          (debug-context context)
                                                                                          (debug-anon-f)]))))
                                  (let [[_ f-args f-body] f-form]
                                    (when (nil? f-body)
                                      (throw (RuntimeException. (str "Cannot resolve call to function: " op " " f-form))))
                                    (seval** (push-context sub-context
                                                           (zipmap f-args
                                                                   args))

                                             f-body))))))))
           (symbol? e) (if (is-free-v? context e)
                         e
                         (resolve-in-context context e))
           (set? e) (set (map (partial seval** context) e))
           (map? e) (process-map (partial seval** context) e)
           (vector? e) (vec (map (partial seval** context) e))
           (string? e) e
           (number? e) e
           (keyword? e) e
           (boolean? e) e))
       (catch Throwable t
         (println (str "failure processing: " [e
                                               (debug-context context)
                                               (debug-anon-f)]))
         (throw t))))

(defn- seval** [context e]
  (let [f (fn [e]
            (seval* context e))]
    (loop [last e
           current (f e)
           n 20]
      (if (or (= last current)
              (zero? n))
        current
        (recur current (f current) (dec n))))))

;;

(defn- simplify-impossible-union [context e]
  (let [[op x y] e]
    (when (and (= op '=)
               (listy? x)
               (expr-with-free-vars? context x)
               (not (expr-with-free-vars? context y))
               (set? y)
               (= (count x) 3))
      (let [[op2 x-2 y-2] x]
        (when (and (= op2 'union)
                   (expr-with-free-vars? context x-2)
                   (not (expr-with-free-vars? context y-2))
                   (set? y-2)
                   (not (subset? y-2 y)))
          false)))))

(defn- simplify-binary-commutative [context f inverse-f op-f operands-good?-f expand-arg-f e]
  (let [[op x y & more] e]
    (when (and (not (nil? x))
               (not (nil? y))
               (empty? more)
               (expr-with-free-vars? context x)
               (not (expr-with-free-vars? context y))
               (listy? x))
      (let [[x-op x-arg1 x-arg2 & more2] x]
        (when (and (= f x-op)
                   (not (nil? x-arg1))
                   (not (nil? x-arg2))
                   (empty? more2)
                   (expr-with-free-vars? context x-arg1)
                   (not (expr-with-free-vars? context x-arg2))
                   (operands-good?-f y x-arg2))
          (let [expanded-arg (expand-arg-f x-arg2)
                clauses (map (fn [z]
                               `(~(op-f op x-arg2) ~x-arg1 (~inverse-f ~y ~z))) expanded-arg)]
            `(~'or ~@clauses)))))))

(defn- simplify-unary-minus [context e]
  (let [[op x y & more] e]
    (when (and (not (nil? x))
               (not (nil? y))
               (empty? more)
               (expr-with-free-vars? context x)
               (not (expr-with-free-vars? context y))
               (listy? x))
      (let [[x-op x-arg1 & more2] x]
        (when (and (= '- x-op)
                   (not (nil? x-arg1))
                   (empty? more2)
                   (expr-with-free-vars? context x-arg1))
          `(~(condp = op
               '> '<=
               '>= '<
               '< '>=
               '<= '>
               op) ~x-arg1 (~'- ~y)))))))

(defn- simplify-difference-on-right [context e]
  (let [[op x y & more] e]
    (when (and (= op '=)
               (not (nil? x))
               (not (nil? y))
               (empty? more)
               (expr-with-free-vars? context x)
               (not (expr-with-free-vars? context y))
               (listy? x))
      (let [[x-op x-arg1 x-arg2 & more2] x]
        (when (and (= 'difference x-op)
                   (not (nil? x-arg1))
                   (empty? more2)
                   (expr-with-free-vars? context x-arg2))
          (when (and (set? y)
                     (set? x-arg1))
            (if (subset? y x-arg1)
              `(~'subset? ~(difference x-arg1 y) ~x-arg2)
              false)))))))

(defn- simplify-into-on-right [context e]
  (let [[op x y & more] e]
    (when (and (= op '=)
               (not (nil? x))
               (not (nil? y))
               (empty? more)
               (expr-with-free-vars? context x)
               (not (expr-with-free-vars? context y))
               (listy? x))
      (let [[x-op x-arg1 x-arg2 & more2] x]
        (when (and (= 'into x-op)
                   (not (nil? x-arg1))
                   (empty? more2)
                   (expr-with-free-vars? context x-arg2))
          (when (and (vector? y)
                     (vector? x-arg1))
            (if (= (take (count x-arg1) y)
                   x-arg1)
              `(~'= ~x-arg2 ~(vec (drop (count x-arg1) y)))
              false)))))))

(defn- simplify-into-on-left [context e]
  (let [[op x y & more] e]
    (when (and (= op '=)
               (not (nil? x))
               (not (nil? y))
               (empty? more)
               (expr-with-free-vars? context x)
               (not (expr-with-free-vars? context y))
               (listy? x))
      (let [[x-op x-arg1 x-arg2 & more2] x]
        (when (and (= 'into x-op)
                   (not (nil? x-arg2))
                   (empty? more2)
                   (expr-with-free-vars? context x-arg1))
          (when (and (vector? y)
                     (vector? x-arg2))
            (if (= (drop (- (count y) (count x-arg2)) y)
                   x-arg2)
              `(~'= ~x-arg1 ~(vec (take (- (count y) (count x-arg2)) y)))
              false)))))))

(defn- simplify-expt [context e]
  (let [[op x y & more] e]
    (when (and (not (nil? x))
               (not (nil? y))
               (empty? more)
               (expr-with-free-vars? context x)
               (not (expr-with-free-vars? context y))
               (listy? x))
      (let [[x-op x-arg1 x-arg2 & more2] x]
        (when (and (= 'expt x-op)
                   (not (nil? x-arg2))
                   (empty? more2)
                   (number? x-arg2)
                   (number? y)
                   (expr-with-free-vars? context x-arg1))
          (let [answer? (expt y (/ 1 x-arg2))]
            (when (and (= 0.0
                          (- answer? (int answer?)))
                       (= 0.0
                          (- y (expt answer? x-arg2))))
              `(~op ~x-arg1 ~(int answer?)))))))))

(defn- simplify-move-free-to-left [context e]
  (let [[op x y & args] e]
    (when (and (not (nil? x))
               (not (nil? y))
               (empty? args)
               (expr-with-free-vars? context y)
               (not (expr-with-free-vars? context x)))
      (apply list (condp = op
                    '> '<=
                    '>= '<
                    '< '>=
                    '<= '>
                    op) [y x]))))

(defn- simplify-not [context e]
  (let [[op x y & more] e]
    (when (and (not (nil? x))
               (not (nil? y))
               (empty? more)
               (expr-with-free-vars? context x)
               (not (expr-with-free-vars? context y))
               (listy? x))
      (let [[x-op x-arg1 & more2] x]
        (when (and (= 'not x-op)
                   (not (nil? x-arg1))
                   (empty? more2)
                   (expr-with-free-vars? context x-arg1))
          `(~'= ~x-arg1 (~'not ~y)))))))

(defn- simplify-binary-minus [context e]
  (let [[_ x y & more] e]
    (when (and (not (nil? x))
               (not (nil? y))
               (empty? more)
               (expr-with-free-vars? context y)
               (not (expr-with-free-vars? context x))
               (not (and (listy? y)
                         (= '- (first y)))))
      `(~'+ ~x (~'- ~y)))))

(defn- flip-inequality [op x]
  (if (neg? x)
    (condp = op
      '> '<
      '> '<
      '<= '>=
      '>= '<=
      op)
    op))

(defn- op-unchanged [op x]
  op)

(defn- operands-good [x y]
  true)

(defn- identity-expand-args [x-arg2]
  #{x-arg2})

(defmacro rule->>
  "Return the first non-nil produced by invoking each of the rules on e in order. If none, then return
  e."
  [e rule & more]
  `(loop [rule# ~rule
          more# ~(vec more)]
     (let [r# (rule# ~e)]
       (if (or (empty? more#)
               (not (nil? r#)))
         (if (nil? r#)
           ~e
           r#)
         (recur (first more#) (rest more#))))))

;;

(defn simple=? [e]
  (and (listy? e)
       (= (first e) '=)
       (symbol? (second e))))

(defn simple=no-expr? [context e]
  (and (listy? e)
       (= 3 (count e))
       (= (first e) '=)
       (symbol? (second e))
       (not (expr-with-free-vars? context (second (rest e))))))

(defn parse-simple= [e]
  (let [[_ s v] e]
    {s v}))

;;

(defn- simplify-and-via-substitution [context e]
  (let [[op & clauses] e
        substitutions (->> clauses
                           (filter (partial simple=no-expr? context))
                           (map parse-simple=)
                           (reduce merge {}))]
    (when (and (= op 'and)
               (not (empty? substitutions)))
      (let [new-context (push-context context substitutions)
            result `(~'and ~@(->> clauses
                                  (map #(if (simple=no-expr? context %)
                                          %
                                          (seval** new-context %)))))]
        (when-not (= result e)
          result)))))

(defn- simplify-EXCEPT-impossible [context e]
  (let [[op x y] e]
    (when (and (#{'= 'not=} op)
               (listy? x)
               (expr-with-free-vars? context x)
               (not (expr-with-free-vars? context y))
               (map? y)
               (= (count x) 4))
      (let [[x-op m path value] x]
        (when (and (= x-op 'EXCEPT)
                   (expr-with-free-vars? context m)
                   (not (expr-with-free-vars? context path))
                   (not (expr-with-free-vars? context value))
                   (symbol? m)
                   ((if (= '= op)
                      not
                      identity) (= value (get-in y path))))
          false)))))

(defn- parse-=-EXCEPT [context e]
  (when (and (listy? e)
             (= 3 (count e)))
    (let [[op x y] e]
      (when (and (= '= op)
                 (listy? x)
                 (= 4 (count x))
                 (expr-with-free-vars? context x)
                 (map? y)
                 (not (expr-with-free-vars? context y)))
        (let [[op2 m path value] x]
          (when (and (= op2 'EXCEPT)
                     (symbol? m)
                     (expr-with-free-vars? context m)
                     (not (expr-with-free-vars? context path))
                     (not (expr-with-free-vars? context value))
                     (= value (get-in y path)))
            {m {:path path
                :path-value value
                :result y}}))))))

(defn- parse-get* [context e]
  (when (and (listy? e)
             (= 3 (count e)))
    (let [[op x y] e]
      (when (and  (= '= op)
                  (listy? x)
                  (= 3 (count x))
                  (expr-with-free-vars? context x)
                  (not (expr-with-free-vars? context y)))
        (let [[op2 m k] x]
          (when (and (= 'get* op2)
                     (expr-with-free-vars? context m)
                     (not (expr-with-free-vars? context k)))
            {m {:key k
                :key-value y}}))))))

(defn- simplify-EXCEPT-complete [context e]
  (let [[op & clauses] e]
    (when (and (= op 'and)
               (> (count clauses) 1))
      (let [excepts-to-consider (->> clauses
                                     (map (partial parse-=-EXCEPT context))
                                     (reduce merge {}))]
        (when (not (empty? excepts-to-consider))
          (let [gets-to-consider (->> clauses
                                      (map (partial parse-get* context))
                                      (reduce merge {}))]
            (when (not (empty? gets-to-consider))
              (let [except-symbols (set (keys excepts-to-consider))
                    get-symbols (set (keys gets-to-consider))
                    symbols-to-consider (set/intersection except-symbols get-symbols)]
                (when (not (empty? symbols-to-consider))
                  (let [substitutions (->> symbols-to-consider
                                           (map #(do
                                                   (let [{:keys [path path-value result]} (get excepts-to-consider %)
                                                         {:keys [key key-value]} (get gets-to-consider %)]
                                                     (when (and (= 1 (count path))
                                                                (= (first path) key))
                                                       {% (assoc result key key-value)}))))
                                           (reduce merge {}))]
                    (when (not (empty? substitutions))
                      (let [new-context (push-context context substitutions)]
                        `(~'and ~@(into (->> clauses
                                             (map #(seval** new-context %)))
                                        (->> substitutions
                                             (map (fn [[k v]]
                                                    `(~'= ~k ~v))))))))))))))))))

(defn apply-rules [context e]
  (let [r (cond
            (listy? e) (let [args (vec (map (partial apply-rules context) (rest e)))
                             [op] e]
                         (cond
                           (= 'and op)
                           (let [args (remove true? args)]
                             (if (some false? args)
                               false
                               (if (empty? args)
                                 true
                                 (if (= 1 (count args))
                                   (first args)
                                   (rule->> (apply list 'and args)
                                            (partial simplify-and-via-substitution context)
                                            (partial simplify-EXCEPT-complete context))))))

                           (= 'or op)
                           (let [args (remove false? args)]
                             (if (empty? args)
                               false
                               (if (some true? args)
                                 true
                                 (if (= 1 (count args))
                                   (first args)
                                   (apply list 'or args)))))

                           (= '+ op)
                           (let [e (rule->> (apply list op args)
                                            (partial simplify-move-free-to-left context))
                                 {numbers true
                                  other false} (group-by number? (rest e))
                                 total (apply + numbers)]
                             (if (and (zero? total)
                                      (= 1 (count other)))
                               (first other)
                               (if (zero? total)
                                 (apply list '+ other)
                                 (apply list '+ (into other [total])))))

                           (= '* op)
                           (rule->> (apply list op args)
                                    (partial simplify-move-free-to-left context))

                           (= 'union op)
                           (rule->> (apply list op args)
                                    (partial simplify-move-free-to-left context))

                           (= '- op)
                           (rule->> (apply list op args)
                                    (partial simplify-binary-minus context))

                           (#{'= 'not=} op)
                           (rule->> (apply list op args)
                                    (partial simplify-move-free-to-left context)
                                    (partial simplify-impossible-union context)

                                    (partial simplify-binary-commutative context 'union 'difference op-unchanged (fn [x y]
                                                                                                                   (subset? y x)) SUBSET)
                                    (partial simplify-binary-commutative context 'difference 'union op-unchanged operands-good SUBSET)
                                    (partial simplify-difference-on-right context)
                                    (partial simplify-binary-commutative context '+ '- op-unchanged operands-good identity-expand-args)
                                    (partial simplify-binary-commutative context '- '+ op-unchanged operands-good identity-expand-args)
                                    (partial simplify-binary-commutative context '* 'div op-unchanged operands-good identity-expand-args)
                                    (partial simplify-binary-commutative context 'div '* op-unchanged operands-good identity-expand-args)
                                    (partial simplify-expt context)
                                    (partial simplify-unary-minus context)
                                    (partial simplify-not context)
                                    (partial simplify-into-on-right context)
                                    (partial simplify-into-on-left context)
                                    (partial simplify-EXCEPT-impossible context))

                           (#{'< '<= '> '>=} op)
                           (rule->> (apply list op args)
                                    (partial simplify-move-free-to-left context)
                                    (partial simplify-binary-commutative context '+ '- op-unchanged operands-good identity-expand-args)
                                    (partial simplify-binary-commutative context '- '+ op-unchanged operands-good identity-expand-args)
                                    (partial simplify-binary-commutative context '* 'div flip-inequality operands-good identity-expand-args)
                                    (partial simplify-binary-commutative context 'div '* flip-inequality operands-good identity-expand-args)
                                    (partial simplify-expt context)
                                    (partial simplify-unary-minus context))

                           :default
                           (apply list op args)))
            (symbol? e) e
            (set? e) (set (map (partial apply-rules context) e))
            (map? e) (process-map (partial apply-rules context) e)
            (vector? e) (vec (map (partial apply-rules context) e))
            (string? e) e
            (number? e) e
            (keyword? e) e
            (boolean? e) e)]
    r))

;;

(defn- sort-unique
  [s]
  (apply list (apply sorted-set-by (fn [x y] (compare (str x) (str y))) (into [] s))))

(defn- collapse-and [e]
  (apply list (conj (sort-unique (let [[_ & args] e]
                                   (loop [[n & more] args
                                          result '()]
                                     (if (nil? n)
                                       result
                                       (if (and (listy? n)
                                                (= 'and (first n)))
                                         (recur more (into result (rest n)))
                                         (recur more (into result [n])))))))
                    'and)))

(defn- collapse-or [e]
  (apply list (conj (sort-unique (let [[_ & args] e]
                                   (loop [[n & more] args
                                          result '()]
                                     (if (nil? n)
                                       result
                                       (if (and (listy? n)
                                                (= 'or (first n)))
                                         (recur more (into result (rest n)))
                                         (recur more (into result [n])))))))
                    'or)))

(defn collapse-and-or [e]
  (cond
    (listy? e) (condp = (first e)
                 'and (collapse-and (apply list (first e) (map collapse-and-or (rest e))))
                 'or (collapse-or (apply list (first e) (map collapse-and-or (rest e))))
                 (apply list (first e) (map collapse-and-or (rest e))))
    (symbol? e) e
    (set? e) (set (map collapse-and-or e))
    (map? e) (zipmap (map collapse-and-or (keys e))
                     (map collapse-and-or (vals e)))
    (vector? e) (vec (map collapse-and-or e))
    (string? e) e
    (number? e) e
    (keyword? e) e
    (boolean? e) e))

(defn seval [context e]
  (binding [*anon-f* (atom {})]
    (let [f (fn [e]
              (->> e
                   (seval* context)
                   (apply-rules context)
                   collapse-and-or))]
      (loop [last e
             current (f e)
             n 20]
        (if (or (= last current)
                (zero? n))
          current
          (recur current (f current) (dec n)))))))

;;

(defn parse-defn-form [f]
  (if (and (> (count f) 3)
           (string? (nth f 2)))
    (let [[_ f-name docstring args & body] f]
      {:f-name f-name
       :docstring docstring
       :args args
       :body body})
    (let [[_ f-name args & body] f]
      {:f-name f-name
       :args args
       :body body})))

(defn- load-defs [x]
  (if (listy? x)
    (let [[op] x]
      (condp = op
        'defn (let [{:keys [f-name args body]} (parse-defn-form x)]
                {f-name `(~'fn ~args ~(last body))})
        'def (let [[_ id binding] x]
               {id binding})
        'VARIABLE {op (rest x)}
        nil))))

(defn load-text
  [text-in]
  (->> (read-string (str "[" text-in "]"))
       (map load-defs)
       (remove nil?)
       (reduce merge {})))

;;

(defn simple-and? [e]
  (and (listy? e)
       (= (first e) 'and)
       (every? simple=? (rest e))))

(defn simple-or? [e]
  (and (listy? e)
       (= (first e) 'or)
       (every? simple-and? (rest e))))

(defn =-to-state [e]
  (let [[_ x v] e]
    {x v}))

(defn and-to-state [ks e]
  (let [[_ & c] e
        result (->> c
                    (map =-to-state)
                    (reduce merge {}))]
    (if ks
      (select-keys result ks)
      result)))

(defn or-to-state [ks e]
  (let [[_ & c] e]
    (->> c
         (map (partial and-to-state ks))
         set)))

(defn to-state [ks e]
  (cond
    (simple-and? e) (and-to-state ks e)
    (simple-or? e) (or-to-state ks e)
    :default e))

(defn- prime-variable? [x]
  (and (symbol? x)
       (.endsWith (name x) "'")))

(defn- delta-state-single [s-ref s-new]
  (reduce merge {} (for [k' (keys s-new)]
                     (let [k (if (prime-variable? k')
                               (symbol (apply str (butlast (name k'))))
                               (symbol (str (name k') "'")))]
                       (when-not (= (s-ref k) (s-new k'))
                         {k' (s-new k')})))))

(defn delta-state [s-ref new-states]
  (if (map? new-states)
    (delta-state-single s-ref new-states)
    (if (and (set? new-states)
             (every? map? new-states))
      (->> new-states
           (map (partial delta-state-single s-ref))
           set)
      new-states)))

;;

(defn simplify*
  [src-file-name constants state e result-formatting vs-to-omit]
  (let [defs-from-file (load-text (slurp src-file-name))
        vs (->> (get defs-from-file 'VARIABLE)
                (filter symbol?))
        prime-vs (->> vs
                      (map #(symbol (str (name %) "'")))
                      vec)
        free-vs (set/difference (set/union (set vs) (set prime-vs)) (keys state))
        trimmed-free-vs (-> (set free-vs)
                            (set/difference (set vs-to-omit))
                            (set/difference (->> vs-to-omit
                                                 (map #(symbol (str % "'")))
                                                 set)))
        context (make-context free-vs (merge (dissoc defs-from-file 'VARIABLE)
                                             constants
                                             state
                                             `{~'VARS- ~vs}))
        result (seval context e)]

    (condp = result-formatting
      nil result
      :raw result
      :states (to-state trimmed-free-vs result)
      :delta (->> result
                  (to-state trimmed-free-vs)
                  (delta-state state)))))

(defn simulate**
  [free-vs constants state n e]
  (let [run-f (fn [m simulate?] (let [context (make-context free-vs (merge constants state m))
                                      context ((if simulate?
                                                 allow-simulation
                                                 clear-simulation) context)]
                                  (let [result (seval context e)
                                        values @*simulated-vars*]
                                    (clear-simulation context)
                                    {:result result
                                     :values values})))]
    (let [r (loop [{:keys [result values]} (run-f {} true)
                   results #{}
                   counter n]
              (if (pos? counter)
                (recur (run-f {} true) (if (true? result)
                                         (conj results values)
                                         results)
                       (dec counter))
                results))]
      (->> r
           (map #(when-not (true? (:result (run-f % false)))
                   (throw (RuntimeException. (str "failed on: " %)))))
           doall)
      r)))

(defn simulate*
  [src-file-name constants state n e result-formatting vs-to-omit]
  (let [defs-from-file (load-text (slurp src-file-name))
        vs (->> (get defs-from-file 'VARIABLE)
                (filter symbol?))
        prime-vs (->> vs
                      (map #(symbol (str (name %) "'")))
                      vec)
        free-vs (set/difference (set/union (set vs) (set prime-vs)) (keys state))
        trimmed-free-vs (-> (set free-vs)
                            (set/difference (set vs-to-omit))
                            (set/difference (->> vs-to-omit
                                                 (map #(symbol (str % "'")))
                                                 set)))
        total-state (merge (dissoc defs-from-file 'VARIABLE)
                           constants
                           state
                           `{~'VARS- ~vs})]
    (simulate** free-vs constants total-state n e)))
