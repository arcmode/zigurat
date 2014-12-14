(ns zigurat.nlp
  (:require [opennlp.treebank :refer [make-treebank-parser]]
            [clojure.string   :refer [lower-case]]))

;; macros
(defmacro if-apply [subject test? then else]
  `(if (~test? ~subject) (~then ~subject) (~else ~subject)))

;; OpenNLP tools
(def treebank-parser (make-treebank-parser "models/en-parser-chunking.bin"))

(defprotocol NLP
  "Natural Language Processing tools."
  (top [nlp-node] "top")
  (jj [nlp-node] "jj"))

(deftype NLPNode [path token-name]
  Object
  (toString [node] token-name)
  NLP
  (top [node] node)
  (jj [node] node))

(def sym->NLPNode (comp (partial ->NLPNode []) name))

(declare ^:private read-node)

(defn name-node [node]
  (symbol (lower-case (name node))))

(defn read-tree [[node & children]]
  (lazy-seq (conj (map read-node children) (name-node node))))

(defn- read-node [node]
  (if-apply node seq? read-tree sym->NLPNode))

(def tree-maker (partial map (comp read-tree read-string)))

(def tree-parser (comp vec tree-maker treebank-parser))

;; (evaluable-tree (TOP (NP rural)))
;; (defmacro evaluable-tree
;;   [form]
;;    `(read-tree '~form))

(time
 (let [x ["rural schools in Santiago of Chile."]]
   (dotimes [_ 100]
     (tree-parser x))))

(-> ["rural"]
    tree-parser)

(top (jj (->NLPNode [] "rural")))

(identity (->NLPNode [] "rural"))

(->> ["rural"]
    tree-parser
    first)

(->> ["rural schools in Santiago of Chile"]
     treebank-parser)
