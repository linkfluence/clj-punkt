(defproject clj-punkt "1.1.0-SNAPSHOT"
  :description "Clojure implementation of the Punkt sentence splitting system. See http://www.mitpressjournals.org/doi/abs/10.1162/coli.2006.32.4.485"
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [slingshot "0.12.2"]]
  :profiles {:dev {:dependencies [[expectations "2.0.9"]]
                   :plugins [[lein-expectations "0.0.7"]]}})
