(ns zigurat.nlp
  (:require [opennlp.treebank :refer [make-treebank-parser]]
            [clojure.string   :refer [lower-case]]

            [zigurat.types.word            :refer [->Word]]))

;; simpler flow control
(defmacro if-apply [subject test? then else]
  `(if (~test? ~subject) (~then ~subject) (~else ~subject)))

;; OpenNLP tools
(def treebank-parser (make-treebank-parser "models/en-parser-chunking.bin"))

;; building the semantic tree

(declare ^:private read-node)

(def name-node (comp symbol lower-case name))

(defn read-tree [[node & children]]
  (lazy-seq (conj (map read-node children) (name-node node))))

(defn leaf-to-word-expr [leaf] `(->Word ~(name leaf)))

(defn- read-node [node] (if-apply node seq? read-tree leaf-to-word-expr))

(def str->tree (comp read-tree read-string))

(def make-tree (partial map str->tree))

(def parse-tree (comp vec make-tree treebank-parser))

