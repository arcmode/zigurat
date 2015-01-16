(ns zigurat.types.sem-graph
  (:require
   [zigurat.protocols.react-graph :refer :all]
   [zigurat.protocols.grammar     :refer :all]
   [clojure.set                   :refer [union]]))

(defrecord SemanticGraph [link nodes edges]
  Grammar
  (top [graph] graph)

  ReactiveGraph
  (bind-with-node
   [graph node]
   (bind-graph-to-node (:link graph) graph node))
  (bind-with-edge
   [graph edge]
   (bind-graph-to-edge (:link graph) graph edge)))

(defrecord SemanticGraphEdge [id labels attrs from to]
  Grammar
  (pp
   [edge elem]
   (bind-with-edge elem edge))

  ReactiveGraph
  (bind-with-node
   [edge node]
   (bind-with-edge node edge))
  (bind-graph-to-node
   [edge graph node]
   (let
     [new-node  (assoc node :out  (conj (:out node)  (:id edge)))
      new-edge  (assoc edge :from (conj (:from edge) (:id node)))
      new-nodes (assoc (:nodes graph) (:id new-node) new-node)
      new-edges (assoc (:edges graph) (:id new-edge) new-edge)
      new-graph (assoc graph :link new-node :nodes new-nodes :edges new-edges)]
     new-graph)))

(defrecord SemanticGraphNode [id labels attrs in out]
  Grammar
  (np  [node] node)
  (np  [node elem]
       (bind-with-node elem node))

  ReactiveGraph
  (bind-with-node
   [node other-node]
   (let [new-labels (union (:labels node) (:labels other-node))
         new-attrs  (merge (:attrs node)  (:attrs other-node))
         new-in     (union (:in node)     (:in other-node))
         new-out    (union (:out node)    (:out other-node))]
     (assoc other-node :labels new-labels :attrs new-attrs :in new-in :out new-out)))

  (bind-with-edge
   [node edge]
   (let [node-id  (:id node)
         edge-id  (:id edge)
         new-node (assoc node :id node-id :in #{edge-id})
         new-edge (assoc edge :id edge-id :to #{node-id})]
     (->SemanticGraph new-edge {node-id new-node} {edge-id new-edge})))

  (bind-graph-to-edge
   [node graph edge]
   (let [new-node  (assoc node :in (conj (:in node) (:id edge)))
         new-edge  (assoc edge :to (conj (:to edge) (:id node)))
         new-nodes (assoc (:nodes graph) (:id new-node) new-node)
         new-edges (assoc (:edges graph) (:id new-edge) new-edge)
         new-graph (assoc graph :link new-edge :nodes new-nodes :edges new-edges)]
     new-graph)))

;; Coordinating conjunctions
(deftype SemanticGraphCoor [label])
