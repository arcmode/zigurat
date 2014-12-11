(ns zigurat.core
  "A translator from natural language forms to the cypher query language.

  * A sentence is a list of phrases
  * A phrase is a hashmap with `:phrase` and `:tag` members"

  (:require [zigurat.data     :refer :all]
            [zigurat.nlp      :refer :all]
            [inflections.core :refer [singular]]
            [clojure.string   :refer [capitalize]]))

(defn resolve-default
  {:test (fn []
           (assert (= {:name "Santiago"}
                      (resolve-default "Santiago")))
           (assert (= :Root
                      (resolve-default "roots"))))}
  [token]
  (if (Character/isUpperCase (first token))
    {:name token}
    (-> token singular capitalize keyword)))

(defn make-token-mapper
  {:test (fn []
           (assert (= {:name "Santiago"}
                      ((make-token-mapper {}) "" "Santiago")))
           (assert (= {:phrase-tag "PP", :token "in", :data "-[:IN*]->", :schema {:in {:PP "-[:IN*]->"}}}
                      ((make-token-mapper {:in {:PP "-[:IN*]->"}}) "PP" "in"))))}
  [schm]
  (fn [phrase-tag token]
    (let [token-key (keyword token)
          tag-key (keyword phrase-tag)
          data (tag-key (token-key schm))]
      (if-let [data (tag-key (token-key schm))]
        {:phrase-tag phrase-tag :token token :data data :schema schm}
        (resolve-default token)))))

(defn make-phrase-mapper
  {:test (fn []
           (assert (= '(["NP" "an"] ["NP" "air"] ["NP" "battle"])
                      ((make-phrase-mapper (fn [a b](vector a b))) {:phrase ["an" "air" "battle"], :tag "NP"}))))}
  [token-mapper]
  (fn [phrase]
    (let [tokens (:phrase phrase)
          phrase-tag (:tag phrase)
          map-token (partial token-mapper phrase-tag)]
      (map map-token tokens))))

(defn make-mapper
  [schm]
  (let [token-mapper (make-token-mapper schm)
        phrase-mapper (make-phrase-mapper token-mapper)
        sentence-mapper (partial map phrase-mapper)]
    (fn [sentences]
      (map sentence-mapper sentences))))
