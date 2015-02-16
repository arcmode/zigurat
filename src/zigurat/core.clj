(ns zigurat.core
  "A translator from natural language forms to the cypher query language.

  * A sentence is a list of phrases
  * A phrase is a hashmap with `:phrase` and `:tag` members"

  (:require [zigurat.nlp                   :refer [parse-tree]]
            [zigurat.protocols.react-graph :refer :all]
            [zigurat.types.sem-graph       :refer :all]
            [clojure.pprint                :refer [pprint]]))

(def map-eval (partial map eval))
(def realize-tree (comp map-eval parse-tree))


;;
;; Word Level
;;

(defrecord JJ  [token])
(defrecord NNS [token])
(defrecord IN  [token])
(defrecord NNP [token])

(defmacro jj  [word] `(->JJ  ~(name word)))
(defmacro nns [word] `(->NNS ~(name word)))
(defmacro in  [word] `(->IN  ~(name word)))
(defmacro nnp [word] `(->NNP ~(name word)))


;;
;; Phrase Level
;;

(defrecord NP [data])
(defrecord PP [data])

;;
;; Grammar methods
;;

(defn top [elem] elem)

(defn class-map
  [& elems]
  (map class elems))

(defmulti np class-map)
(defmulti pp class-map)

(defmethod np
  [zigurat.core.JJ zigurat.core.NNS]
  [jj nns]
  (let [node-id (gensym "n")
        labels  #{(:token jj) (:token nns)}
        attrs   {}
        in      #{}
        out     #{}
        node    (->SemanticGraphNode node-id labels attrs in out)]
    (->NP node)))

(defmethod np
  [zigurat.core.NNP]
  [nnp]
  (let [node-id (gensym "n")
        labels  #{}
        attrs   {:name (:token nnp)}
        in      #{}
        out     #{}
        node    (->SemanticGraphNode node-id labels attrs in out)]
    (->NP node)))

(defmethod np
  [zigurat.core.NP zigurat.core.PP]
  [np pp]
  (->NP (bind-node-to-source (:data pp) (:data np))))

(defmethod pp
  [zigurat.core.IN zigurat.core.NP]
  [in np]
  (let [np-data   (:data np)
        link-node (get-node np-data)
        edge-id   (gensym "e")
        labels    #{(:token in)}
        attrs     {}
        from      #{}
        to        #{(:id link-node)}
        edge      (->SemanticGraphEdge edge-id labels attrs from to)
        graph     (bind-incoming-edge np-data edge)]
    (->PP graph)))
