(ns salt.state
  "Keeps track of the state as salt source is transpiled into TLA+."
  (:require [clojure.set :as set]))

;; helpers

(defn push-stack [x v]
  (conj x v))

(defn pop-stack [x]
  (vec (butlast x)))

(defn peek-stack [x]
  (last x))

;; c-state

(defn new-c-state []
  {:context [:TOP] ;; used as a stack to keep track of the surrounding context as the AST is walked
   :constants #{} ;; set of symbols declared as CONSTANTS in the spec
   :base-variables [] ;; vector of symbols declared as VARIABLES in the spec (does not include prime
                      ;; symbols)
   :variables-stack [#{}] ;; a stack of sets of symbols that are in scope
   :functions #{} ;; set of all symbols that correspond to functions or maps defined in the spec
   :function-arg-count {} ;; map from function symbol to the number of arguments the function
                          ;; expects
   :constant-functions #{}
   :f-non-constant nil ;; temporary flag that gets set whenever a variable is accessed, then at the
                       ;; top after a function has been transpiled we can understand whether it was
                       ;; a constant function or not
   :invocation-count {} ;; count of number of args passed into functions when they are invoked, used
                        ;; when transpiling an invocation
   :invocation-position-count {}
   :indent 0 ;; level of indentation, i.e. number of spaces, currently at for outputed TLA+ code
   :hanging-indent-stack [] ;; stack of how far the hanging indent is set.
   :column 0 ;; column position currently at based on latest emiited text
   :text "" ;; the body of the transpiled code, i.e. the TLA+ code
   :anonymous-symbol nil ;; records the synbol to use as the argument to the anonymous function that
                         ;; is currently in scope
})

(def ^:dynamic *c-state* (atom new-c-state))

(defn reset-c-state []
  (reset! *c-state* (new-c-state)))

(defn push-context [context]
  (when (not (#{:CONSTANT :VARIABLE :ASSUME 'ns 'defn 'and 'or} context))
    (throw (RuntimeException. (str "Unexpected context value: " context))))
  (swap! *c-state* #(assoc % :context (push-stack (:context %) context))))

(defn pop-context []
  (swap! *c-state* #(assoc % :context (pop-stack (:context %)))))

(defn context []
  (last (:context @*c-state*)))

;; text formatting

(defn space-for-tabs
  ([]
   (space-for-tabs false))
  ([extra-tab?]
   (apply str (repeat (+ (:indent @*c-state*)
                         (if extra-tab? 4 0)
                         (or (peek-stack (:hanging-indent-stack @*c-state*))
                             0)) " "))))

(defn get-column
  ([]
   (get-column false))
  ([extra-tab?]
   (+ (:column @*c-state*)
      (if extra-tab? 4 0))))

(defn get-text []
  (:text @*c-state*))

(defn set-column [new-column]
  (swap! *c-state* #(assoc % :column new-column)))

(defn tab []
  (swap! *c-state* #(assoc % :indent (+ (:indent %) 4))))

(defn start-hanging-indent []

  (swap! *c-state* #(let [indent-amount (- (:column %)
                                           (:indent %))]
                      (assoc % :hanging-indent-stack (push-stack (:hanging-indent-stack %) indent-amount)))))

(defn untab []
  (swap! *c-state* #(assoc % :indent (max 0
                                          (- (:indent %) 4)))))

(defn stop-hanging-indent []
  (swap! *c-state* #(assoc % :hanging-indent-stack (pop-stack (:hanging-indent-stack %)))))

(defn clear-text []
  (swap! *c-state* #(assoc % :text "")))

(defn set-text [current-text]
  (swap! *c-state* #(assoc % :text current-text)))

(defn emit* [s]
  (swap! *c-state* #(assoc % :text (str (:text %) s))))

(defn prefix-text [module-str]
  (swap! *c-state* #(let [t (:text %)]
                      (assoc % :text (str module-str t)))))

;; functions

(defn known-function? [f]
  ((:functions @*c-state*) f))

(defn known-constant-function? [f]
  ((:constant-functions @*c-state*) f))

(defn known-constant? [c]
  ((:constants @*c-state*) c))

(defn known-variable? [x]
  ((peek-stack (:variables-stack @*c-state*)) x))

(defn set-f-recursive []
  (swap! *c-state* #(assoc % :f-recursive true)))

(defn is-f-recursive? []
  (:f-recursive @*c-state*))

(defn is-current-f-name? [x]
  (= x (:f-name @*c-state*)))

(defn compiling-function? []
  (boolean (:f-name @*c-state*)))

(defn function-takes-args? [f]
  (pos? (or ((:function-arg-count @*c-state*) f)
            0)))

(defn clear-f-recursive []
  (swap! *c-state* #(dissoc % :f-recursive)))

(defn set-function-arg-count [f-name n]
  (swap! *c-state* #(assoc-in % [:function-arg-count f-name] n)))

(defn is-f-non-constant? []
  (:f-non-constant @*c-state*))

(defn clear-f-non-constant []
  (swap! *c-state* #(dissoc % :f-non-constant)))

(defn set-f-non-constant []
  (swap! *c-state* #(assoc % :f-non-constant true)))

(defn add-constant-function [f-name]
  (swap! *c-state* #(assoc % :constant-functions (conj (:constant-functions %) f-name))))

(defn clear-f-name []
  (swap! *c-state* #(dissoc % :f-name)))

(defn set-f-name [f-name]
  (swap! *c-state* #(assoc % :f-name f-name)))

(defn add-function [f-name]
  (swap! *c-state* #(assoc % :functions (conj (:functions %) f-name))))

(defn set-invocation-arg-count [f-name arg-count]
  (swap! *c-state* #(assoc-in % [:invocation-count f-name] arg-count)))

(defn get-invocation-arg-count [f-name]
  (get-in @*c-state* [:invocation-count f-name]))

(defn clear-invocation-arg-count []
  (swap! *c-state* #(assoc % :invocation-count {})))

(defn set-invocation-position-arg-count [f-name position arg-count]
  (swap! *c-state* #(assoc-in % [:invocation-position-count f-name position] arg-count)))

(defn get-invocation-position-arg-count [f-name position]
  (get-in @*c-state* [:invocation-position-count f-name position]))

(defn set-anonymous-symbol [s]
  (swap! *c-state* #(assoc-in % [:anonymous-symbol] s)))

(defn clear-anonymous-symbol []
  (swap! *c-state* #(assoc-in % [:anonymous-symbol] nil)))

(defn is-anonymous-symbol? [s]
  (= (get-in @*c-state* [:anonymous-symbol]) s))

;; constants and variables

(defn set-constants [constants]
  (swap! *c-state* #(assoc % :constants (set constants))))

(defn push-vars [v]
  (swap! *c-state* #(let [starting-stack (:variables-stack %)
                          current-variables (peek-stack starting-stack)
                          new-variables (set/union current-variables (set v))]
                      (-> %
                          (assoc :variables-stack (push-stack starting-stack new-variables))))))

(defn pop-vars []
  (swap! *c-state* #(let [starting-stack (:variables-stack %)]
                      (-> %
                          (assoc :variables-stack (pop-stack starting-stack))))))

(defn set-prime-variables [variables]
  (push-vars (set/union (set variables)
                        (map (fn [v]
                               (symbol (str v "'"))) variables))))

(defn get-base-variables-except [vs]
  (->> (:base-variables @*c-state*)
       (remove (set vs))
       vec))

(defn set-base-variables [variables]
  (swap! *c-state* #(assoc % :base-variables variables)))
