(defproject zigurat "0.1.0-SNAPSHOT"
  :description "A translator from english to cypher."
  :url "http://github.com/drojas/zigurat"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [clojure-opennlp "0.3.2"]]
  :profiles {:dev {:dependencies []}}
  :test-refresh {:notify-command ["notice"]
                 :notify-on-success true})
