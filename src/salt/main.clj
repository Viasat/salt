(ns salt.main
  (:require [salt.transpiler :as transpiler]))

(defn -main [& args]
  (let [[input-file-name] args]
    (print (transpiler/transpile-text (slurp input-file-name)))))
