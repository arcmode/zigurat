(defproject zigurat "0.1.0-SNAPSHOT"
  :description "A translator from english to cypher."
  :url "http://github.com/drojas/zigurat"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure    "1.6.0"]
                 [clojure-opennlp        "0.3.2"]
                 [org.clojure/core.match "0.3.0-alpha4"]]
  :profiles {:dev {:dependencies [[doctest              "0.1.1"]
                                  [clojurewerkz/balagan "1.0.0"]]
                   :plugins [[com.jakemccrary/lein-test-refresh "0.5.2"]]}}
  :test-refresh {:notify-on-success true})
