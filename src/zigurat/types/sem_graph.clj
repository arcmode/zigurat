(ns zigurat.types.sem-graph
  (:require
   [zigurat.protocols.react-graph :refer :all]
   [clojure.set                   :refer [union]]))

;;
;; Semantic Graph Elements
;;

(defrecord SemanticGraph [link nodes edges]
  ReactiveGraph
  (bind-node-to-source
   [graph node]
   (let [link-edge   (get-edge link)
         new-edge-id (:id link-edge)
         new-node-id (:id node)
         new-node    (update-in node      [:out]  conj new-edge-id)
         new-edge    (update-in link-edge [:from] conj new-node-id)
         new-nodes   (assoc nodes new-node-id new-node)
         new-edges   (assoc edges new-edge-id new-edge)
         new-link    new-node]
     (assoc graph :link new-link :nodes new-nodes :edges new-edges)))
  (get-node
   [graph]
   (get-node link))
  (bind-incoming-edge
   [graph edge]
   (let [link-node   (get-node link)
         new-node-id (:id link-node)
         new-edge-id (:id edge)
         new-edge    (update-in edge      [:to] conj new-node-id)
         new-node    (update-in link-node [:in] conj new-edge-id)
         new-nodes   (assoc nodes new-node-id new-node)
         new-edges   (assoc edges new-edge-id new-edge)
         new-link    new-edge]
     (assoc graph :link new-link :nodes new-nodes :edges new-edges))))

(defrecord SemanticGraphEdge [id labels attrs from to]
  ReactiveGraph
  (get-edge
   [edge]
   edge))

(defrecord SemanticGraphNode [id labels attrs in out]
  ReactiveGraph
  (get-node
   [node]
   node)
  (bind-incoming-edge
   [node edge]
   (let [new-edge-id (:id edge)
         new-edge    (update-in edge [:to] conj id)
         new-node    (update-in node [:in] conj new-edge-id)
         link         new-edge
         nodes       {id new-node}
         edges       {new-edge-id new-edge}]
     (->SemanticGraph link nodes edges))))
