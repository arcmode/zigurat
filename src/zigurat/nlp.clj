(ns zigurat.nlp
  (:require [opennlp.treebank :refer [make-treebank-parser]]
            [clojure.string   :refer [lower-case]]
            [clojure.set      :refer [union]]))

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

(defn- read-node [node] (if-apply node seq? read-tree name))

(def str->tree (comp read-tree read-string))

(def make-tree (partial map str->tree))

(def parse-tree (comp vec make-tree treebank-parser))

(def realize-tree (comp (partial map eval) parse-tree))

;; building the graph of facts

;; [todo] split into graph, edge and node protocols
(defprotocol ReactiveGraph
  "A protocol for reactive graphs."
  (bond-with-node [elem node])
  (bond-with-edge [elem edge])
  (bond-graph-to-node [elem graph node])
  (bond-graph-to-edge [elem graph edge]))

;; [todo] split into categories
(defprotocol Grammar
  "A protocol for grammars."
  (top [phrase])
  (np  [phrase]
       [phrase-a phrase-b])
  (pp  [phrase-a phrase-b]))

(defrecord SemGraphEdge [id labels attrs from to]
  Grammar
  (pp [edge elem]
      (bond-with-edge elem edge))

  ReactiveGraph
  (bond-with-node [edge node]
                  (bond-with-edge node edge))
  (bond-graph-to-node [edge graph node]
                      (let [new-node (assoc node :out (conj (:out node) (:id edge)))
                            new-edge (assoc edge :from (conj (:from edge) (:id node)))
                            new-nodes (assoc (:nodes graph) (:id new-node) new-node)
                            new-edges (assoc (:edges graph) (:id new-edge) new-edge)
                            new-graph (assoc graph :link new-node :nodes new-nodes :edges new-edges)]
                        new-graph)))

(defrecord SemGraph [link nodes edges]
  Grammar
  (top [graph] graph)

  ReactiveGraph
  (bond-with-node [graph node]
                  (bond-graph-to-node (:link graph) graph node))
  (bond-with-edge [graph edge]
                  (bond-graph-to-edge (:link graph) graph edge)))

(defrecord SemGraphNode [id labels attrs in out]
  Grammar
  (np  [node] node)
  (np  [node elem]
       (bond-with-node elem node))

  ReactiveGraph
  (bond-with-node [node other-node]
                  (let [new-labels (union (:labels node) (:labels other-node))
                        new-attrs (merge (:attrs node) (:attrs other-node))
                        new-in (union (:in node) (:in other-node))
                        new-out (union (:out node) (:out other-node))]
                    (assoc other-node
                      :labels new-labels
                      :attrs new-attrs
                      :in new-in
                      :out new-out)))
  (bond-with-edge [node edge]
                  (let [node-id (:id node)
                        edge-id (:id edge)
                        new-node (assoc node :id node-id :in #{edge-id})
                        new-edge (assoc edge :id edge-id :to #{node-id})]
                    (->SemGraph new-edge
                             {node-id new-node}
                             {edge-id new-edge})))
  (bond-graph-to-edge [node graph edge]
                      (let [new-node (assoc node :in (conj (:in node) (:id edge)))
                            new-edge (assoc edge :to (conj (:to edge) (:id node)))
                            new-nodes (assoc (:nodes graph) (:id new-node) new-node)
                            new-edges (assoc (:edges graph) (:id new-edge) new-edge)
                            new-graph (assoc graph :link new-edge :nodes new-nodes :edges new-edges)]
                        new-graph)))

;; building the graph

(defn jj  [word] (->SemGraphNode (gensym "n") #{word} {} #{} #{}))
(defn nns [word] (->SemGraphNode (gensym "n") #{word} {} #{} #{}))
(defn in  [word] (->SemGraphEdge (gensym "e") #{:in} {} #{} #{}))
(defn nnp [word] (->SemGraphNode (gensym "n") #{} {:name word} #{} #{}))


(np (->SemGraphNode (gensym "n") #{:location} {:name "Chile"}  #{} #{})
    (->SemGraphNode (gensym "n") #{:country} {:alias "Chilito"} #{} #{}))

(in "of")

(np (nnp "Chile"))

(np (nnp "Santiago"))

(np (nnp "Santiago"))

(pp (in "of") (np (nnp "Chile")))

(np (np (nnp "Santiago"))
    (pp (in "of")
        (np (nnp "Chile"))))

(pp (in "in")
    (np (np (nnp "Santiago"))
        (pp (in "of")
            (np (nnp "Chile")))))

(np (jj "rural")
  (nns "schools"))

(np (np (jj "rural")
        (nns "schools"))
    (pp (in "in")
        (np (np (nnp "Santiago"))
            (pp (in "of")
                (np (nnp "Chile"))))))


(top (np (np (jj "rural")
             (nns "schools"))
         (pp (in "in")
             (np (np (nnp "Santiago"))
                 (pp (in "of")
                     (np (nnp "Chile")))))))



(realize-tree ["rural schools in Santiago of Chile"])

(class (realize-tree ["rural schools in Santiago of Chile"]))

(time
 (let [x ["rural schools in Santiago of Chile"]]
   (dotimes [_ 100]
     (realize-tree x))))

(time
 (let [x (parse-tree ["rural schools in Santiago of Chile"])]
   (dotimes [_ 100]
     (map eval x))))

(time
 (let [x ["rural schools in Santiago of Chile"]]
   (dotimes [_ 100]
     (np (np (jj "rural")
             (nns "schools"))
         (pp (in "in")
             (np (np (nnp "Santiago"))
                 (pp (in "of")
                     (np (nnp "Chile")))))))))

;; (time
;;  (let [x ["Malloco is a rural location in Central Chile" "rural schools in Santiago of Chile"]]
;;    (dotimes [_ 100]
;;      (eval-tree '(np (np (jj "rural")
;;                     (nns "schools"))
;;                 (pp (in "in")
;;                     (np (np (nnp "Santiago"))
;;                         (pp (in "of")
;;                             (np (nnp "Chile"))))))))))


;; (time
;;  (let [x ["Malloco is a rural location in Central Chile" "rural schools in Santiago of Chile"]]
;;    (dotimes [_ 100]
;;      (eval (parse-tree x)))))

(-> ["rural schools in Santiago of Chile ."]
    parse-tree
    )

(-> ["rural schools in Santiago of Chile having more than five teachers with a PHD ."]
    parse-tree
    )

(-> ["A pizza with lots of pepperoni, pineapple, capsicum, mushrooms, anchovies, olives, and vegemite ."]
    parse-tree)

(comment
(top (s (s (s (np (prp "He"))
              (vp (vbd "ate")
                  (np (dt "a")
                      (nn "pizza"))
                  (pp (in "with")
                      (np (np (nns "lots"))
                          (pp (in "of")
                              (np (nn "pepperoni")
                                  (nn "pineapple")
                                  (nn "capsicum")))))))
           (vp (md "mushrooms")
               (vp (vb "anchovies")
                   (adjp (jj "olives")))))
        (cc "and")
        (s (np (nn "vegemite")))
        (. ".")))
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
;;     parse-tree
;;      eval)
