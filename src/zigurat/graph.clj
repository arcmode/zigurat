(ns zigurat.graph)

;;
;; Graph Protocols
;;

(defprotocol ReactiveNode
  (get-graphnode       [elem])
  (get-node            [elem])
  ;; rename to take-incoming-edge ?
  (bind-incoming-edge  [elem edge]))

(defprotocol ReactiveEdge
  (get-graphedge       [elem])
  (get-edge            [elem])
  ;; rename to take-input-node ?
  (bind-node-to-source [elem node]))

(defprotocol TraversableGraph
  (nodes [elem])
  (edges [elem]))

(defprotocol PropertyGraph
  (attrs  [elem])
  (labels [elem])
  (in     [elem])
  (out    [elem])
  (from   [elem])
  (to     [elem]))

;;
;; Semantic Graph Elements
;;

(declare ->GraphNode)
(declare ->GraphEdge)

(defrecord GraphEdge [link-id nodes edges]
  ReactiveEdge
  (get-graphedge
   [graph]
   graph)
  (get-edge
   [_]
   (link-id edges))
  (bind-node-to-source
   [graph graphnode]
   (let [edge      (link-id edges)
         node-id   (:link-id graphnode)
         node      (node-id (:nodes graphnode))
         new-node  (update-in node [:out]  conj link-id)
         new-edge  (update-in edge [:from] conj node-id)
         new-nodes (merge nodes (:nodes graphnode) {node-id new-node})
         new-edges (merge edges (:edges graphnode) {link-id new-edge})]
     (->GraphNode node-id new-nodes new-edges))))

(defrecord GraphNode [link-id nodes edges]
  ReactiveNode
  (get-graphnode
   [graph]
   graph)
  (get-node
   [_]
   (link-id nodes))
  (bind-incoming-edge
   [graph graphedge]
   (let [node      (link-id nodes)
         edge-id   (:link-id graphedge)
         edge      (edge-id (:edges graphedge))
         new-edge  (update-in edge [:to] conj link-id)
         new-node  (update-in node [:in] conj edge-id)
         new-nodes (merge nodes (:nodes graphedge) {link-id new-node})
         new-edges (merge edges (:edges graphedge) {edge-id new-edge})]
     (->GraphEdge edge-id new-nodes new-edges))))

(def default-traversable-graph-impl
  {:nodes (fn [elem] (map second (:nodes elem)))
   :edges (fn [elem] (map second (:edges elem)))})

(extend GraphNode
  TraversableGraph
  default-traversable-graph-impl)

(extend GraphEdge
  TraversableGraph
  default-traversable-graph-impl)

(def default-property-graph-impl
  {:attrs  (fn [elem] (:attrs  elem))
   :labels (fn [elem] (:labels elem))})

(def default-property-node-impl
  (merge default-property-graph-impl
         {:in  (fn [elem] (:in  elem))
          :out (fn [elem] (:out elem))}))

(def default-property-edge-impl
  (merge default-property-graph-impl
         {:from (fn [elem] (:from elem))
          :to   (fn [elem] (:to   elem))}))

(defrecord Node [id labels attrs in out]
  ReactiveNode
  (get-graphnode
   [node]
   (->GraphNode id {id node} {})))

(defrecord Edge [id labels attrs from to]
  ReactiveEdge
  (get-graphedge
   [edge]
   (->GraphEdge id {} {id edge})))

;; Transition states (don't know (where to put/how to call) this feature yet)
(defrecord Node-  [code])
(defrecord Edge-> [code])

(extend Node
  PropertyGraph
  default-property-node-impl)

(extend Edge
  PropertyGraph
  default-property-edge-impl)

(extend Node-
  PropertyGraph
  default-property-node-impl)

(extend Edge->
  PropertyGraph
  default-property-edge-impl)


;; Helpers
;;

(defn make-node
  [{:keys [id labels attrs in out]
    :or {id     (gensym "n")
         labels #{}
         attrs   {}
         in     #{}
         out    #{}}}]
  (->Node id labels attrs in out))

(defn make-edge
  [{:keys [id labels attrs from to]
    :or {id     (gensym "e")
         labels #{}
         attrs   {}
         from   #{}
         to     #{}}}]
  (->Edge id labels attrs from to))

(defn make-graph-node
  [{:keys [link-id nodes edges]
    :or {nodes {}
         edges {}}}]
  (->GraphNode link-id nodes edges))

(defn make-graph-edge
  [{:keys [link-id nodes edges]
    :or {nodes {}
         edges {}}}]
  (->GraphEdge link-id nodes edges))
