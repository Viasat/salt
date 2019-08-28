(ns salt
  "This module is the main entry point for doing something with a salt specification."
  (:require [salt.simplify :as simplify]
            [salt.transpiler :as transpiler]))

(defmacro evaluate
  "Create a context in which constants are defined as in 'constants' and variables are defined as in
  'state', then evaluate the 'body'. All constants and variables must be defined."
  [constants state & body]
  `(with-redefs ~(into [] (mapcat identity (merge constants state)))
     ~@body))

(defmacro simplify
  "Read a salt source file, create a context in which constants are defined as per the 'constants'
  parameter and the variables are defined per the 'state' format. Then simplify the expression, 'e'
  in that context. Additional optional arguments consist of 'result-formatting' followed by
  'vars-to-omit'. Apply 'result-formatting' to the resulting expression, with options
  being :raw (simply return the resulting expression), :states (convert the expression into either a
  single map or set of maps that represents the variable values that make the expression true)
  or :delta (which is the same as :states, except it will omit variables which are the same as the
  corresponding values from the initial 'state'). By default the output will be formatted as if
  the :raw format is indicated. Optionally include a final parameter, 'vars-to-omit' which is a
  vector of symbols to omit from the resulting state representations. All constants must be
  defined. A subset of the variables can be defined."
  [src-file-name constants state e & arg]
  `(simplify/simplify* ~src-file-name '~constants '~state '~e ~(first arg) '~(second arg)))

(defmacro with-rand-seed [n & body]
  `(binding [simplify/*rand-seed-atom* (atom ~n)]
     ~@body))

(defmacro simulate
  [src-file-name constants state n e & arg]
  `(simplify/simulate* ~src-file-name '~constants '~state ~n '~e ~(first arg) '~(second arg)))

(defn transpile
  "Read a salt source file, convert the contents to TLA+ tokens and return the results as a string."
  [src-file-name]
  (transpiler/transpile-text (slurp src-file-name)))

;;

(defn- find-namespace [ns-symbol]
  (first (filter #(= ns-symbol (ns-name %)) (all-ns))))

(defn namespace-fixture
  "Used for testing salt specifications. (NOTE: Should not need to exist, but need to make some
  changes before it can be removed.)"
  [ns-symbol]
  (fn [f]
    (binding [*ns* (find-namespace ns-symbol)]
      (f))))
