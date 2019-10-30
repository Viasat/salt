(defproject org.clojars.david-mcneil/salt "0.0.4"
  :description "Converts a subset of clojure into TLA+"
  :url "https://github.com/Viasat/salt"
  :signing {:gpg-key "david@david-mcneil.com"}
  :dependencies [[org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/algo.generic "0.1.3"]
                 [org.clojure/clojure "1.10.0"]
                 [org.clojure/math.combinatorics "0.1.5"]]
  :plugins [[lein-cljfmt "0.5.7"]])
