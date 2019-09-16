(ns salt.transpiler
  "Walks a tree of salt (i.e. clojure) code and transpiles into the corresponding TLA+ code."
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [salt.lang :as lang]
            [salt.transpiler-state :as state]))

;; helper functions

(defn- ends-in-newline-ignoring-spaces? [s]
  (if (= "" s)
    false
    (let [n (count s)]
      (loop [i (dec n)]
        (let [c (subs s i (inc i))]
          (if (= " " c)
            (if (pos? i)
              (recur (dec i))
              false)
            (= "\n" c)))))))

(defn- ends-in-boolean-operator? [s]
  (or (re-matches #".*/\\ *" (last (string/split s #"\n")))
      (re-matches #".*\\/ *" (last (string/split s #"\n")))
      (re-matches #".* IF *" (last (string/split s #"\n")))
      (re-matches #".* IN *" (last (string/split s #"\n")))))

(defn- count-trailing-spaces [s]
  (count (last (re-find #"[^ ]*( +)$" s))))

(defn- count-trailing-chars [s]
  (let [trailing (last (re-find #"[\n]([^\n]*)$" s))]
    (when trailing
      (count trailing))))

(defn- emit-helper [column s]
  (let [s1 (string/replace s "\n" (str "\n" (state/space-for-tabs)))
        char-count (if (ends-in-newline-ignoring-spaces? s1)
                     0
                     (count s1))
        [s2 new-column] (if (or (ends-in-newline-ignoring-spaces? s1)
                                (ends-in-boolean-operator? s1))
                          [s1 (count-trailing-spaces s1)]
                          (if (and (> (+ char-count column) 77)
                                   (not (or (= "," s1)
                                            (= ", " s1)
                                            (= ">>" s1)
                                            (= " " s1)
                                            (= ")" s1))))
                            [(str "\n" (state/space-for-tabs true) s1)
                             (state/get-column true)]
                            [s1 (+ column (count s1))]))
        s3 (if (or (and (> new-column 120)
                        (not (or (= "," s1)
                                 (= ", " s1)
                                 (= ">>" s1)
                                 (= " " s1)
                                 (= ")" s1)))))
             (= "\n/\\  " s2)
             s2)
        new-column (if (ends-in-newline-ignoring-spaces? s3)
                     (count-trailing-spaces s2)
                     (let [trailing-count (count-trailing-chars s3)]
                       (if trailing-count
                         trailing-count
                         new-column)))]
    {:s s3
     :new-column new-column}))

(defn- emit [s0]
  (let [column (state/get-column)
        {:keys [s new-column]} (emit-helper column (str s0))]
    (state/emit* s)
    (state/set-column new-column)))

(defn- newline-before? []
  (let [t (state/get-text)
        t (subs t (max (- (count t) 200) 0))]
    (boolean (or (= t "")
                 (ends-in-newline-ignoring-spaces? t)
                 (ends-in-boolean-operator? t)))))

(def transpile)

(defn- expr
  ([x]
   (expr x false))
  ([x has-parens?]
   (let [use-parens (and (list? x)
                         (not (#{'CHOOSE
                                 'Nat
                                 'first
                                 'rest
                                 'fm-
                                 'fn
                                 'Cardinality
                                 'DOMAIN
                                 'if
                                 'and
                                 'or
                                 'let
                                 'count
                                 'EXCEPT
                                 'get*
                                 'maps-} (first x)))
                         (not (and (= 2 (count x))
                                   (keyword? (first x))))
                         (not (let [f (first x)]
                                (or (state/known-function? f)
                                    (state/known-constant? f)
                                    (state/known-variable? f)))))]
     (when (and use-parens
                (not has-parens?))
       (emit "("))
     (transpile x)
     (when (and use-parens
                (not has-parens?))
       (emit ")")))))

(defn- is-comment? [x]
  (and (list? x)
       (= 'comment (first x))))

(defn- f-with-separator
  [f before-first separator after-last x]
  (loop [items x
         counter 0]
    (let [e (first items)
          comment? (is-comment? e)]
      (if (not (nil? e))
        (do
          (cond
            comment? nil
            (zero? counter) (emit before-first)
            (pos? counter) (emit (if (string? separator)
                                   separator
                                   (separator))))
          (f e)
          (recur (rest items)
                 (if comment?
                   counter
                   (inc counter))))
        (when (pos? counter)
          (emit after-last))))))

(defn- transpile-with-separator [before-first separator after-last x]
  (f-with-separator transpile before-first separator after-last x))

(defn- expr-with-separator [before-first separator after-last x]
  (f-with-separator expr before-first separator after-last x))

(defn- unary-operator
  ([x operator]
   (unary-operator x operator false))
  ([x operator mandatory-parens]
   (if (= 2 (count x))
     (let [[_ t] x]
       (emit operator)
       (when (and (not mandatory-parens)
                  (not (#{"~"} operator)))
         (emit " "))
       (when mandatory-parens
         (emit "(")
         (when (not (#{"Head" "Tail"} operator))
           (emit " ")))
       (state/tab)
       (expr t mandatory-parens)
       (state/untab)
       (when mandatory-parens
         (when (not (#{"Head" "Tail"} operator))
           (emit " "))
         (emit ")")))
     (throw (RuntimeException. (str "wrong number of args: " x))))))

(defn- list-with-no-child-lists? [x]
  (and (list? x)
       (not (some list? (rest x)))
       (or (not (= '= (first x)))
           (not (= '== (first x))))))

(defn- binary-operator
  ([x operator]
   (binary-operator x operator true))
  ([x operator use-spaces?]
   (if (= 3 (count x))
     (let [[_ a b] x]
       (if (and (list-with-no-child-lists? a)
                (#{"=" "=="} operator))
         (transpile a)
         (expr a))
       (emit (str (if use-spaces? " " "")
                  operator
                  (if use-spaces? " " "")))
       (state/tab)
       (if (and (list-with-no-child-lists? b)
                (#{"=" "=="} operator))
         (transpile b)
         (expr b))
       (state/untab))
     (throw (RuntimeException. (str "wrong number of args: " x))))))

(defn- n-ary-operator
  [x operator]
  (let [[_ a b c] x]
    (emit (str operator "("))
    (state/tab)
    (expr a)
    (emit ", ")
    (expr b)
    (when c
      (emit ", ")
      (expr c))
    (state/untab)
    (emit ")")))

(defn- reverse-binary-operator [x operator]
  (let [[op a b] x]
    (binary-operator [op b a] operator)))

;; transpile

;; transpile list forms

(defmulti transpile-list first)

(defmethod transpile-list 'ns [x]
  (let [[_ module-name] x
        module-str (str "---------------------------- MODULE "
                        module-name
                        " ----------------------------\n")]
    (state/prefix-text module-str))
  (state/push-context 'ns)
  (->> (rest x)
       (map transpile)
       (doall))
  (state/pop-context))

(defmethod transpile-list :require [x]
  (when (= 'ns (state/context))
    (let [r-f (fn [r]
                (condp = (first r)
                  'tlaplus.Integers "Integers"
                  'tlaplus.Sequences "Sequences"
                  'tlaplus.Naturals "Naturals"
                  'tlaplus.FiniteSets "FiniteSets"
                  nil))
          extends (->> (rest x)
                       (map r-f)
                       (remove nil?))]
      (when (seq extends)
        (emit "EXTENDS")
        (f-with-separator emit " " ", " "" extends)
        (emit "\n\n")))))

(defn- end-of-module []
  (emit "\n")
  (emit "=============================================================================")
  (emit "\n"))

(defmethod transpile-list 'CONSTANT [x]
  (let [constants (vec (remove (comp not symbol?) (rest x)))
        children (rest x)]
    (state/set-constants constants)
    (emit "CONSTANT")
    (state/tab)
    (state/push-context :CONSTANT)
    (transpile-with-separator " " ", " "" children)
    (state/untab)
    (emit "\n\n")
    (state/pop-context)))

(defmethod transpile-list 'ASSUME [x]
  (let [assumptions (rest x)]
    (emit "ASSUME")
    (state/push-context :ASSUME)
    (state/tab)
    (emit "\n")
    (state/untab)
    (f-with-separator #(do
                         (state/tab)
                         (transpile %)
                         (state/untab)) "" "\n" "" assumptions)
    (emit "\n\n")
    (state/pop-context)))

(defmethod transpile-list 'VARIABLE [x]
  (let [variables (vec (remove (comp not symbol?) (rest x)))
        children (rest x)]
    (state/set-prime-variables variables)
    (state/set-base-variables variables)
    (emit "VARIABLE")
    (state/tab)
    (state/push-context :VARIABLE)
    (transpile-with-separator " " ", " "" children)
    (state/push-vars variables)
    (state/untab)
    (emit "\n\n")
    (state/pop-context)))

(defmethod transpile-list '== [x]
  (binary-operator x "="))

(defmethod transpile-list 'CHANGED- [x]
  (let [[op vs] x]
    (unary-operator [op (state/get-base-variables-except vs)] "UNCHANGED")))

(defmethod transpile-list 'UNCHANGED [x]
  (unary-operator x "UNCHANGED"))

(defn- emit-get-arg [k]
  (emit "[")
  (transpile k)
  (emit "]"))

(defmethod transpile-list 'line- [x]
  (state/untab)
  (emit "--------------------------------------------------------------------------------\n"))

(defn- line-count [s]
  (count (string/split s #"\n")))

(defmethod transpile-list 'comment [x]
  (let [[_ s] x]
    (when (#{'and 'or} (state/context))
      (state/untab))
    (when (not (newline-before?))
      (emit "\n"))
    (emit "\n")
    (if (= 1 (line-count s))
      (do
        (emit (str "\\* " s))
        (emit "\n"))
      (do
        (emit "(*")
        (emit "\n")
        (state/emit* s)
        (emit "\n")
        (emit "*)")
        (emit "\n"))))
  (when (#{'and 'or} (state/context))
    (state/tab)))

(defn- pull-up-ands [children]
  (->> children
       (mapcat (fn [child]
                 (if (and (list? child)
                          (= (first child) 'and))
                   (rest child)
                   [child])))))

;; primitives

(defn- transpile-number [x]
  (emit x))

(defn- transpile-string [x]
  (emit (str "\"" x "\"")))

(defn- transpile-symbol [x]
  (when (not (= 'ns (state/context)))
    (if (= x 'Nat)
      (emit "Nat")
      (if (= x 'VARS-)
        (transpile (state/get-base-variables-except []))
        (if (state/is-current-f-name? x)
          (do
            (emit (str x))
            (state/set-f-recursive))
          (if (or ({:CONSTANT :VARIABLE} (state/context))
                  (state/known-constant? x)
                  (state/known-variable? x)
                  (state/known-function? x))
            (do
              (when (and (state/compiling-function?)
                         (or (state/known-variable? x)
                             (and (state/known-function? x)
                                  (not (state/known-constant-function? x)))))
                (state/set-f-non-constant))
              (emit (str x)))
            (if (state/is-anonymous-symbol? x)
              (emit "@")
              (throw (RuntimeException. (str "unknown symbol: >" x "<"))))))))))

(defn- transpile-keyword [x]
  (emit (str "\"" (name x) "\"")))

(defn- transpile-true [x]
  (emit "TRUE"))

(defn- transpile-false [x]
  (emit "FALSE"))

;; structure

(defmethod transpile-list 'if [x]
  (if (#{2 3} (count (rest x)))
    (let [[_ c t f] x]
      (when (not (newline-before?))
        (emit "\n"))
      (emit "IF  ")
      (state/tab)
      (expr c)
      (state/untab)
      (when (not (newline-before?))
        (emit "\n"))
      (emit "THEN ")
      (state/tab)
      (expr t)
      (state/untab)
      (when f
        (when (not (newline-before?))
          (emit "\n"))
        (emit "ELSE ")
        (state/tab)
        (expr f)
        (state/untab)))
    (throw (RuntimeException. (str "wrong number of args: " x)))))

(defmethod transpile-list 'let [x]
  (let [[_ bindings & body] x]
    (when (not (newline-before?))
      (emit "\n"))
    (emit "LET")
    (state/tab)
    (state/push-vars (->> (partition 2 bindings)
                          (map first)))
    (->> (partition 2 bindings)
         (f-with-separator (fn [[x v]]
                             (emit x)
                             (emit " == ")
                             (expr v)) " " "\n" "")
         doall)
    (state/untab)
    (when (not (newline-before?))
      (emit "\n"))
    (emit "IN  ")
    (f-with-separator #(do
                         (state/tab)
                         (transpile %)
                         (state/untab)) "" "\n" "" body)
    (state/pop-vars)))

(defmethod transpile-list 'cond [x]
  (let [[_ & args] x
        cases (partition 2 args)]
    (emit "CASE")
    (f-with-separator (fn [[condition result]]
                        (if (= condition :default)
                          (emit "OTHER")
                          (expr condition))
                        (emit " -> ")
                        (expr result)) "\n" "\n[] " "\n" cases)))

;; logic

(defmethod transpile-list 'and [x]
  (state/push-context 'and)
  (when (not (newline-before?))
    (emit "\n"))
  (emit "")
  (let [[_ & clauses] x]
    (if (> (count clauses) 1)
      (f-with-separator #(do
                           (state/tab)
                           (transpile %)
                           (state/untab))
                        "/\\  "
                        #(if (newline-before?)
                           "/\\  "
                           "\n/\\  ")
                        "" (pull-up-ands (rest x)))
      (transpile (first clauses))))
  (state/pop-context))

(defmethod transpile-list 'or [x]
  (state/push-context 'or)
  (when (not (newline-before?))
    (emit "\n"))
  (emit "")
  (f-with-separator #(do
                       (state/tab)
                       (transpile %)
                       (state/untab)) "\\/  "
                    #(if (newline-before?)
                       "\\/  "
                       "\n\\/  ")
                    "" (rest x))
  (state/pop-context))

(defmethod transpile-list 'not [x]
  (if (= 1 (count (rest x)))
    (if (and (list? (second x))
             (= 'contains? (first (second x))))
      (reverse-binary-operator (second x) "\\notin")
      (unary-operator x "~" false))
    (throw (RuntimeException. (str "wrong number of args: " x)))))

(defmethod transpile-list '=> [x]
  (binary-operator x "=>"))

(defmethod transpile-list '<=> [x]
  (binary-operator x "<=>"))

(defmethod transpile-list '= [x]
  (binary-operator x "="))

(defmethod transpile-list 'not= [x]
  (binary-operator x "#"))

(defmethod transpile-list '> [x]
  (binary-operator x ">"))

(defmethod transpile-list '< [x]
  (binary-operator x "<"))

(defmethod transpile-list '>= [x]
  (binary-operator x ">="))

(defmethod transpile-list '<= [x]
  (binary-operator x "<="))

(defmethod transpile-list 'E [x]
  (let [[_ [v r] & body] x]
    (state/push-vars [v])
    (emit "\\E ")
    (transpile v)
    (emit " \\in ")
    (expr r)
    (emit " :")
    (state/tab)
    (emit "\n")
    (state/untab)
    (f-with-separator #(do
                         (state/tab)
                         (transpile %)
                         (state/untab)) "" "\n" "" body)
    (state/pop-vars)))

(defmethod transpile-list 'A [x]
  (let [[_ [v r] & body] x]
    (state/push-vars [v])
    (emit "\\A ")
    (transpile v)
    (emit " \\in ")
    (expr r)
    (emit " :")
    (state/tab)
    (emit "\n")
    (state/untab)
    (f-with-separator #(do
                         (state/tab)
                         (transpile %)
                         (state/untab)) "" "\n" "" body)
    (state/pop-vars)))

;; temporal logic

(defmethod transpile-list 'always- [x]
  (let [[_ f vars] x]
    (if vars
      (do
        (emit (str "[]["))
        (transpile f)
        (emit (str "]_"))
        (transpile vars))
      (let [_ f]
        (emit (str "[]"))
        (emit "(")
        (transpile f)
        (emit ")")))))

(defmethod transpile-list 'WF [x]
  (let [[_ vars f] x]
    (emit (str "WF_"))
    (emit vars)
    (emit "(")
    (transpile f)
    (emit ")")))

(defmethod transpile-list 'SF [x]
  (let [[_ vars f] x]
    (emit (str "SF_"))
    (emit vars)
    (emit "(")
    (transpile f)
    (emit ")")))

(defmethod transpile-list 'eventually- [x]
  (let [[_ f vars] x]
    (emit (str "<>"))
    (transpile f)))

(defmethod transpile-list 'leads-to- [x]
  (let [[_ p q] x]
    (expr p)
    (emit " ~> ")
    (expr q)))

;; arithmetic

(defmethod transpile-list '+ [x]
  (binary-operator x "+"))

(defmethod transpile-list '- [x]
  (binary-operator x "-"))

(defmethod transpile-list '* [x]
  (binary-operator x "*"))

(defmethod transpile-list 'div [x]
  (binary-operator x "\\div"))

(defmethod transpile-list 'mod* [x]
  (binary-operator x "%"))

(defmethod transpile-list 'expt [x]
  (binary-operator x "^" false))

(defmethod transpile-list 'Nat [x]
  (emit "Nat"))

;; set functions

(defn- transpile-set [x]
  (let [n (count x)
        separator (if (> n 1) " " "")]
    (emit "{")
    (state/tab)
    (expr-with-separator separator ", " separator x)
    (state/untab)
    (emit "}")))

(defmethod transpile-list 'range* [x]
  (binary-operator x ".." false))

(defmethod transpile-list 'difference [x]
  (binary-operator x "\\"))

(defmethod transpile-list 'union [x]
  (binary-operator x "\\union"))

(defmethod transpile-list 'X [x]
  ;; Cartesian product
  (binary-operator x "\\X"))

(defmethod transpile-list 'UNION [x]
  ;; Combine many sets
  (unary-operator x "UNION"))

(defmethod transpile-list 'intersection [x]
  (binary-operator x "\\intersect"))

(defmethod transpile-list 'subset? [x]
  (binary-operator x "\\subseteq"))

(defmethod transpile-list 'subset-proper? [x]
  (binary-operator x "\\subset"))

(defmethod transpile-list 'superset? [x]
  (binary-operator x "\\supseteq"))

(defmethod transpile-list 'superset-proper? [x]
  (binary-operator x "\\supset"))

(defmethod transpile-list 'SUBSET [x]
  (unary-operator x "SUBSET"))

(defmethod transpile-list 'contains? [x]
  (reverse-binary-operator x "\\in"))

(defmethod transpile-list 'select [x]
  (let [[_ [_ [v] & body] s] x]
    (when (nil? s)
      (throw (RuntimeException. (str "cannot call select on nil set"))))
    (emit "{ ")
    (state/push-vars [v])
    (transpile v)
    (emit " \\in ")
    (expr s)
    (state/tab)
    (emit " :\n")
    (transpile-with-separator "" "\n" "" body)
    (state/untab)
    (emit " }")))

(defmethod transpile-list 'map* [x]
  (when (not (newline-before?))
    (emit "\n"))
  (let [[_ [_ [v] & body] s] x]
    (emit "{ ")
    (state/push-vars [v])
    (state/tab)
    (transpile-with-separator "" "\n" "" body)
    (emit " :\n")
    (transpile v)
    (emit " \\in ")
    (expr s)
    (state/untab)
    (emit " }")))

(defmethod transpile-list 'Cardinality [x]
  (unary-operator x "Cardinality" true))

(defmethod transpile-list 'CHOOSE [x]
  (let [[_ [v s] condition] x]
    (emit "CHOOSE ")
    (state/tab)
    (state/push-vars [v])
    (transpile v)
    (emit " \\in ")
    (expr s)
    (emit " : ")
    (expr condition)
    (state/pop-vars)
    (state/untab)))

;; TLA+ tuples (i.e. vectors)

(defn- transpile-vector [x]
  (if (empty? x)
    (emit "<< >>")
    (do
      (expr-with-separator "<< " ", " " " x)
      (emit ">>"))))

(defmethod transpile-list 'first [x]
  (unary-operator x "Head" true))

(defmethod transpile-list 'rest [x]
  (unary-operator x "Tail" true))

(defmethod transpile-list 'into [x]
  (binary-operator x "\\o"))

(defmethod transpile-list 'str [x]
  (transpile (apply list 'into (rest x))))

(defmethod transpile-list 'count [x]
  (unary-operator x "Len" true))

(defmethod transpile-list 'SubSeq [x]
  (n-ary-operator x "SubSeq"))

(defmethod transpile-list 'SelectSeq [x]
  (let [[op b a] x]
    (n-ary-operator [op a b] "SelectSeq")))

(defmethod transpile-list 'conj [x]
  (n-ary-operator x "Append"))

(defmethod transpile-list 'Seq [x]
  (unary-operator x "Seq" true))

(defmethod transpile-list 'every?* [x]
  (let [[_ data-set t] x]
    (transpile (list 'contains?
                     (list 'Seq data-set)
                     t))))

;; TLA+ function (and record) functions

(defn- is-tuple? [m]
  (= (set (keys m))
     (set (range 1 (inc (count m))))))

(defn- map-as-tuple [m]
  (vec (map #(get m %) (range 1 (inc (count m))))))

(defn- transpile-map [x]
  (if (and (not (empty? x))
           (is-tuple? x))
    (transpile-vector (map-as-tuple x))
    (do
      (emit "[")
      (state/start-hanging-indent)
      (let [to-sym (fn [k]
                     (when (keyword? k)
                       (->> k name symbol)))]
        (state/push-vars (remove nil? (map to-sym (keys x))))
        (f-with-separator (fn [[k v]]
                            (transpile (or (to-sym k)
                                           k))
                            (emit " |-> ")
                            (state/tab)
                            (transpile v)
                            (state/untab))
                          "" ",\n" "" x)
        (state/stop-hanging-indent)
        (state/pop-vars))
      (emit "]"))))

(defmethod transpile-list 'fm- [x]
  (let [[_ bindings body] x
        bindings (partition 2 bindings)]
    (state/tab)
    (emit "[")
    (->> bindings
         (f-with-separator (fn [[v s]]
                             (emit v)
                             (emit " \\in ")
                             (expr s))
                           ""
                           ", "
                           "")
         doall)
    (emit " |-> ")
    (state/push-vars (map first bindings))
    (expr body)
    (emit "]")
    (state/pop-vars)
    (state/untab)))

(defmethod transpile-list 'defm- [x]
  (let [[_ map-name bindings body] x]
    (emit map-name)
    (emit " == ")
    (transpile (list 'fm-
                     bindings
                     body))
    (emit "\n\n")
    (state/add-function map-name)
    (state/add-constant-function map-name)))

(defmethod transpile-list 'maps- [x]
  (if (= 3 (count x))
    (let [[_ p q] x]
      (emit "[")
      (state/start-hanging-indent)
      (expr p)
      (emit " -> ")
      (expr q)
      (state/stop-hanging-indent)
      (emit "]"))
    (let [[_ pairs] x
          pairs (partition 2 pairs)]
      (emit "[")
      (state/start-hanging-indent)
      (let [to-sym (comp symbol name)]
        (state/push-vars (map to-sym (map first pairs)))
        (f-with-separator (fn [[k v]]
                            (transpile (to-sym k))
                            (emit " : ")
                            (transpile v))
                          "" ",\n" "" pairs)
        (state/stop-hanging-indent)
        (state/pop-vars))
      (emit "]"))))

(defmethod transpile-list 'get* [x]
  (let [[_ m k] x]
    (transpile m)
    (if (keyword? k)
      (emit (str "." (name k)))
      (emit-get-arg k))))

(defn- transpile-lambda [x]
  (let [[_ [v] body] x]
    (state/set-anonymous-symbol v)
    (transpile body)
    (state/clear-anonymous-symbol)))

(defmethod transpile-list 'EXCEPT [x]
  (let [[_ m & bindings] x]
    (emit "[")
    (state/tab)
    (transpile m)
    (emit " EXCEPT")
    (state/tab)
    (loop [[path new-value & more] bindings
           counter 0]
      (when (zero? counter)
        (if (seq? more)
          (emit "\n")
          (emit " ")))
      (when (pos? counter)
        (emit ",\n"))
      (emit "!")
      (f-with-separator emit-get-arg "" "" "" path)
      (emit " = ")
      (if (and (list? new-value)
               (= 'fn* (first new-value)))
        (transpile-lambda new-value)
        (transpile new-value))
      (when (seq? more)
        (recur more (inc counter))))
    (state/untab)
    (state/untab)
    (emit "]")))

(defmethod transpile-list 'DOMAIN [x]
  (unary-operator x "DOMAIN"))

;; TLA+ operators (i.e. functions)

(defn- parse-defn-form [f]
  (if (and (> (count f) 3) (string? (nth f 2)))
    (let [[_ f-name docstring args & body] f]
      {:f-name f-name, :docstring docstring, :args args, :body body})
    (let [[_ f-name args & body] f]
      {:f-name f-name, :args args, :body body})))

(defmethod transpile-list 'defn [x]
  (if (= :TOP (state/context))
    (let [{:keys [f-name docstring args body]} (parse-defn-form x)]
      (if (zero? (count args))
        (do
          ;; if there are no args to the function, just emit it in a straightforward fashion
          (when docstring
            (transpile-list (list 'comment docstring)))
          (state/push-context 'defn)
          (state/clear-invocation-arg-count)
          (state/set-f-name f-name)
          (state/add-function f-name)

          (let [n (count args)]
            (emit f-name)
            (state/set-function-arg-count f-name n))
          (emit " == ")
          (state/tab)
          (->> body
               (map transpile)
               (doall))
          (state/untab)
          (emit "\n")
          (state/clear-f-name)
          (when (not (state/is-f-non-constant?))
            (state/add-constant-function f-name))
          (state/clear-f-non-constant)
          (state/pop-context)
          (state/clear-invocation-arg-count)
          (emit "\n"))
        (do
          ;; if the function takes args then transpile the body first so we can determine whether it
          ;; is recursive and how to deal with higher order functions
          (when docstring
            (transpile-list (list 'comment docstring)))
          (state/push-context 'defn)
          (state/clear-invocation-arg-count)
          (state/set-f-name f-name)
          (state/add-function f-name)
          (let [current-text (state/get-text)]
            (state/clear-text)
            (state/tab)
            (emit "\n")
            (state/push-vars args)
            (->> body
                 (map transpile)
                 (doall))
            (state/pop-vars)
            (state/untab)
            (emit "\n")
            (let [new-text (state/get-text)]
              (let [n (count args)]
                (state/set-text current-text)
                (when (state/is-f-recursive?)
                  (emit (str "RECURSIVE " f-name "("))
                  (f-with-separator emit "" ", " "" (repeat n "_"))
                  (emit ")\n\n")
                  (state/clear-f-recursive))
                (emit f-name)

                (state/set-function-arg-count f-name n)
                (if (pos? n)
                  (->> args
                       (map (fn [position s]
                              (let [invocation-arg-count (state/get-invocation-arg-count s)]
                                (if invocation-arg-count
                                  (do
                                    ;; higher order functions, arguments that are functions need to be handled
                                    (state/set-invocation-position-arg-count f-name position invocation-arg-count)
                                    (apply list (into [s] (repeat invocation-arg-count '_))))
                                  s)))
                            (range))
                       (f-with-separator #(if (list? %)
                                            (do (emit (first %))
                                                (emit "(")
                                                (f-with-separator emit "" ", " "" (rest %))
                                                (emit ")"))
                                            (emit %)) (if (> n 1) "( " "( ") ", " (if (> n 1) " )" " )")))
                  (when (and (state/is-f-non-constant?)
                             (pos? n))
                    (emit "()"))))
              (state/tab)
              (emit " ==")
              (state/untab)
              (if (or (list? (first body))
                      (set? (first body))
                      (vector? (first body))
                      (.contains (string/trim new-text) "\n"))
                (do
                  (emit "\n")
                  (state/emit* (subs new-text 1)))
                (do
                  (emit " ")
                  (state/emit* (string/trim (subs new-text 1)))
                  (emit "\n")))))
          (state/clear-f-name)
          (when (not (state/is-f-non-constant?))
            (state/add-constant-function f-name))
          (state/clear-f-non-constant)
          (state/pop-context)
          (state/clear-invocation-arg-count)
          (emit "\n"))))
    (throw (RuntimeException. "cannot have nested defn"))))

(defmethod transpile-list 'def [x]
  (let [[_ n v] x]
    (transpile-list (apply list ['defn n [] v]))))

(defmethod transpile-list 'fn [x]
  (let [[_ args body] x]
    (state/tab)
    (emit "LAMBDA ")
    (f-with-separator emit " " ", " "" args)
    (emit ": ")
    (state/push-vars args)
    (emit "(")
    (transpile body)
    (emit ")")
    (state/pop-vars)
    (state/untab)))

(defn- invocation [x]
  (let [f (first x)]
    (if (and (keyword? f)
             (not (= 'ns (state/context))))
      (let [[k m] x]
        (transpile (apply list ['get* m k])))
      (if (or (state/known-function? f)
              (state/known-variable? f))
        (let [constant-function? (state/known-constant-function? f)
              use-parens? (or (state/is-current-f-name? f)
                              (state/function-takes-args? f)
                              (state/known-variable? f))]
          (when (and constant-function?
                     (pos? (count (rest x))))
            (throw (RuntimeException. (str "did not expect args to function: " f))))
          (when (state/known-variable? f)
            (state/set-invocation-arg-count f (count (rest x))))
          (transpile f)
          (when use-parens?
            (emit "("))
          (state/tab)
          (f-with-separator (if (= 1 (count (rest x)))
                              transpile
                              expr)
                            ""
                            ", "
                            ""
                            (rest x))
          (->> (rest x)
               (map (fn [position s]
                      (when (and (symbol? s)
                                 (state/known-variable? s)
                                 (not (state/is-current-f-name? f)))
                        (let [n (state/get-invocation-position-arg-count f position)]
                          (when n
                            (state/set-invocation-arg-count s n)))))
                    (range))
               doall)
          (state/untab)
          (when use-parens?
            (emit ")")))
        (transpile-list x)))))

;;

(defn- transpile [x]
  (let [c-f (cond (vector? x) transpile-vector
                  (set? x) transpile-set
                  (list? x) invocation
                  (number? x) transpile-number
                  (string? x) transpile-string
                  (symbol? x) transpile-symbol
                  (= true x) transpile-true
                  (= false x) transpile-false
                  (map? x) transpile-map
                  (keyword? x) transpile-keyword
                  :else (throw (RuntimeException. (str "unknown form: '" x "'"))))]
    (when (list? x))
    (c-f x)))

;; entry points for testing

(defn transpile-form
  "Pass in a clojure data structure, return a string of the corresponding TLA+ code that has
  accumulated so far, including past forms. Used for testing."
  [f]
  (transpile f)
  (state/get-text))

(defn reset
  "Clear the current transpilation state. Used for testing."
  []
  (state/reset-c-state))

(defn transpile-single-form
  "Pass in a clojure data structure, returns a string of the corresponding TLA+ code. Clears the
  transpilation state before running. Used for testing."
  [f]
  (reset)
  (transpile f)
  (state/get-text))

;; main entry point

(defn transpile-text
  "Pass in a string of salt (i.e. clojure code) to produce a string of the corresponding TLA+ code as
  the return value."
  [text-in]
  (state/reset-c-state)
  (->> (read-string (str "[" text-in "]"))
       (map transpile)
       doall)
  (end-of-module)
  (state/get-text))
