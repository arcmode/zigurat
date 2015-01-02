(ns zigurat.types.sem-graph
  (:require [zigurat.protocols.react-graph :refer :all]
            [zigurat.protocols.grammar     :refer :all]

            [clojure.set :refer [union]]))

(defrecord SemanticGraphEdge [id labels attrs from to]
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

(defrecord SemanticGraph [link nodes edges]
  Grammar
  (top [graph] graph)

  ReactiveGraph
  (bond-with-node [graph node]
                  (bond-graph-to-node (:link graph) graph node))
  (bond-with-edge [graph edge]
                  (bond-graph-to-edge (:link graph) graph edge)))

(defrecord SemanticGraphNode [id labels attrs in out]
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
                    (->SemanticGraph new-edge
                             {node-id new-node}
                             {edge-id new-edge})))
  (bond-graph-to-edge [node graph edge]
                      (let [new-node (assoc node :in (conj (:in node) (:id edge)))
                            new-edge (assoc edge :to (conj (:to edge) (:id node)))
                            new-nodes (assoc (:nodes graph) (:id new-node) new-node)
                            new-edges (assoc (:edges graph) (:id new-edge) new-edge)
                            new-graph (assoc graph :link new-edge :nodes new-nodes :edges new-edges)]
                        new-graph)))
