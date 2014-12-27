(ns zigurat.nlp
  (:require [opennlp.treebank :refer [make-treebank-parser
                                      make-treebank-linker]]
            [clojure.string   :refer [lower-case]]
            [clojure.set      :refer [union]]))

;; simpler flow control
(defmacro if-apply [subject test? then else]
  `(if (~test? ~subject) (~then ~subject) (~else ~subject)))

;; OpenNLP tools
(def treebank-parser (make-treebank-parser "models/en-parser-chunking.bin"))

;; building the tree

(declare ^:private read-node)

(def name-node (comp symbol lower-case name))

(defn read-tree [[node & children]]
  (lazy-seq (conj (map read-node children) (name-node node))))

(defn- read-node [node] (if-apply node seq? read-tree name))

(def str->tree (comp read-tree read-string))

(def tree-maker (partial map str->tree))

(def tree-parser (comp vec tree-maker treebank-parser))

;; (defprotocol NLPElemProtocol
;;   "A protocol for primitive nlp elements.

;;   Each method takes a string and returns either a node or edge."
;;   (in  [a])
;;   (jj  [a])
;;   (nns [a])
;;   (nnp [a]))

(defprotocol GraphElement
  (bond-with-node [elem node])
  (bond-with-edge [elem edge]))

(defprotocol NLPNode
  "A protocol for nlp nodes.

  Each method takes a number of nodes|edges|graphs."
  (top [node])
  (np  [node]
       [node elem]))

(defprotocol NLPEdge
  "A protocol for nlp edges.

  Each method takes a number of nodes|edges|graphs."
  (pp  [a b])
  ;(bond-with-node [a b])
  )

;; (defprotocol NLPGraph
;;   "A protocol for nlp graphs.

;;   Each method takes a number of nodes|edges|graphs."
;;   (bond-with-node [a b])
;;   )


(defrecord Edge [labels attrs from to]
  Object
  (toString [edge] (str labels " " attrs))

  NLPEdge
  (pp [edge elem]
      (bond-with-edge elem edge))

  GraphElement
  (bond-with-node [edge node]
                  (bond-with-edge node edge)))

(defrecord Graph [link nodes edges]
  GraphElement
  (bond-with-node [graph node]
                  "hola"))

(defrecord Node [labels attrs in out]
  Object
  (toString [edge] (str labels " " attrs))

  NLPNode
  (np  [node] node)
  (np  [node elem]
       (bond-with-node elem node))

  GraphElement
  (bond-with-node [node other_node]
                  (merge-with union other_node node))
  (bond-with-edge [node edge]
                  (let [node-id (gensym "n")
                        edge-id (gensym "e")
                        new-node (assoc node :id node-id :out #{edge-id})
                        new-edge (assoc edge :id edge-id :from #{node-id})]
                    (->Graph edge
                             {node-id new-node}
                             {edge-id new-edge}))))

;; building the graph

(defn jj  [token] (->Node #{token} {} #{} #{}))
(defn nns [token] (->Node #{token} {} #{} #{}))
(defn in  [token] (->Edge #{:in} {} #{} #{}))
(defn nnp [token] (->Node #{} {:name token} #{} #{}))


(np (->Node #{:location} {:name "Chile"} #{} #{})
    (->Node #{:country} {:alias "Chilito"} #{} #{}))


(in "of")
(np (nnp "Chile"))

(pp (in "of") (np (nnp "Chile")))
(np (nnp "Santiago"))

(np (nnp "Santiago")) (pp (in "of") (np (nnp "Chile")))


(np (np (nnp "Santiago"))
    (pp (in "of")
        (np (nnp "Chile"))))


(top (np (np (jj "rural")
             (nns "schools"))
         (pp (in "in")
             (np (np (nnp "Santiago"))
                 (pp (in "of")
                     (np (nnp "Chile")))))))


;; (time
;;  (let [x ["Malloco is a rural location in Central Chile" "rural schools in Santiago of Chile"]]
;;    (dotimes [_ 100]
;;      (eval (tree-parser x)))))

(-> ["rural schools in Santiago of Chile"]
    tree-parser
    )

(-> ["rural schools in Santiago of Chile having more than five teachers with a PHD"]
    tree-parser
    )

;; (top (np (np (jj "rural")
;;              (nns "schools"))
;;          (pp (in "in")
;;              (np (np (nnp "Santiago"))
;;                  (pp (in "of")
;;                      (np (np (nnp "Chile"))
;;                          (pp (in "with")
;;                              (s (np (qp (jjr "more")
;;                                         (in "than")
;;                                         (cd "five"))
;;                                     (nns "teachers"))
;;                                 (vp (vbg "having")
;;                                     (np (dt "a")
;;                                         (nn "PHD")))))))))))


;; (top (np (np (jj "rural")
;;              (nns "schools")
;;          (pp (in "in")
;;              (np (np (nnp "Santiago"))
;;                  (pp (in "of")
;;                      (np (nnp "Chile"))))))))


;; (seq? {:a :b})


;; {:nodes {:n1 {:labels [:rural :schools]
;;               :out    [:e1]}
;;          :n2 {:attrs  {:name "Santiago"}
;;               :in     [:e1]
;;               :out    [:e2]}
;;          :n3 {:attrs  {:name "Chile"}}}
;;  :edges {:e1 {:labels [:in]
;;               :from   [:n1]
;;               :to     [:n2]}
;;          :e2 {:labels [:in]
;;               :from   [:n2]
;;               :to     [:n3]}}}


;; {:x1 {:type   :node
;;       :labels [:rural :schools]
;;       :out    [:x2]}
;;  :x2 {:type   :edge
;;       :labels [:in]
;;       :in     [:x1]
;;       :out    [:x3]}
;;  :x3 {:type   :node
;;       :attrs  {:name "Santiago"}
;;       :in     [:x2]
;;       :out    [:x4]}
;;  :x4 {:type   :edge
;;       :labels [:in]
;;       :in     [:x3]
;;       :out    [:x5]}
;;  :x5 {:type   :node
;;       :attrs  {:name "Chile"}
;;       :in     [:x4]}}


;; (->> ["rural"]
;;     tree-parser
;;      eval)

