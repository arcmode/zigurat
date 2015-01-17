(ns zigurat.nlp
  (:require
   [opennlp.treebank :refer [make-treebank-parser]]
   [clojure.string   :refer [lower-case]]
   [clojure.pprint   :refer [pprint]]))

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
  (if-apply node seq? read-tree identity))

(def str->tree (comp read-tree read-string))

(def make-trees (partial map str->tree))

(def parse-tree (comp make-trees treebank-parser))

















(treebank-parser ["rural schools located in coastal cities of Chile and with more than fifty students"]
                 )


(comment

  (TOP (S (NP (JJ rural)
            (NNS schools))
        (VP (VP (VBN located)
                (PP (IN in)
                    (NP (NP (JJ coastal)
                            (NNS cities))
                        (PP (IN of)
                            (NP (NNP Chile))))))
            (CC and)
            (VP (PP (IN with)
                    (NP (QP (JJR more)
                            (IN than)
                            (CD fifty))
                        (NNS students)))))))

(pprint (parse-tree ["rural schools located in coastal cities of Chile and with more than fifty students"]
))

(treebank-parser
["rural schools in coastal cities of Chile and with more than fifty students"]
)


(TOP (NP (NP (JJ rural)
             (NNS schools))
         (PP (PP (IN in)
                 (NP (NP (JJ coastal)
                         (NNS cities))
                     (PP (IN of)
                         (NP (NNP Chile)))))
             (CC and)
             (PP (IN with)
                 (NP (QP (JJR more)
                         (IN than)
                         (CD fifty))
                     (NNS students))))))


let run be a function of animals

clients
  with a key named executive

open conversations

;;here \n\t binds to the last node on the previous line
rural schools
  in Santiago of Chile
  with more than ten teachers who have at least one PHD
;;idem
rural schools
  in coastal cities
    of Chile
    with more than twenty coffee shops

;;here \n\t binds to the first node on the previous line
rural schools in Santiago of Chile
  with more than ten teachers who have at least one PHD
;;idem
rural schools
 in coastal cities of Chile
    with more than twenty coffee shops

  (treebank-parser ["rural schools in Santiago of Chile" ""])

  )
