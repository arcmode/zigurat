(ns zigurat.nlp
  (:require [opennlp.treebank :refer [make-treebank-parser]]
            [clojure.string   :refer [lower-case]]))

;; simpler flow control
(defmacro if-apply
  [subject test? then else]
  `(if (~test? ~subject) (~then ~subject) (~else ~subject)))

;; OpenNLP tools
(def treebank-parser (make-treebank-parser "models/en-parser-chunking.bin"))

;; building the semantic tree

(declare ^:private read-node)

(def name-node (comp symbol lower-case name))

;; [todo] refactor to a `make-tree-reader` internalizing `read-node`
(defn read-tree
  [[node & children]]
  (lazy-seq (conj (map read-node children) (name-node node))))

(defn- read-node
  [node]
  (if-apply node seq? read-tree name))

(def str->tree (comp read-tree read-string))

(def make-trees (partial map str->tree))

(def parse-tree (comp make-trees treebank-parser))
