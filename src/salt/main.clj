(ns salt.main
  (:require [salt]))

(defn -main [& args]
  (let [[input-file-name] args]
    (print (salt/transpile input-file-name))))
