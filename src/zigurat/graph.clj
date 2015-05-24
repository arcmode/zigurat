(ns zigurat.graph)

;;
;; Graph Protocols
;;

(defprotocol ReactiveNode
  (get-graphnode       [elem])
  (get-node            [elem])
  ;; rename to take-incoming-edge ?
  (bind-incoming-edge  [elem edge] [elem node next-link-type]))

(defprotocol ReactiveEdge
  (get-graphedge       [elem])
  (get-edge            [elem])
  ;; rename to take-input-node ?
  (bind-node-to-source [elem node] [elem node next-link-type]))

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

(defrecord GraphEdge [link-id nodes edges])
(defrecord GraphNode [link-id nodes edges])
(defrecord Edge      [id labels attrs from to])
(defrecord Node      [id labels attrs in   out])
;; Transition states (don't know how to implement this feature yet)
(defrecord Edge->    [code])
(defrecord Node-     [code])

(def default-traversable-graph-impl
  {:nodes (fn [elem] (map second (:nodes elem)))
   :edges (fn [elem] (map second (:edges elem)))})

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

(def graph-node-reactive-graph-impl
  {:get-graphnode identity
   :get-node (fn
               [graph]
               ((:link-id graph) (:nodes graph)))
   :bind-incoming-edge (fn bind-incoming-edge
                         ([graph graphedge] (bind-incoming-edge graph graphedge :edge-link))
                         ([graph graphedge next-link-type]
                          (let [node-id   (:link-id graph)
                                node      (node-id (:nodes graph))
                                edge-id   (:link-id graphedge)
                                edge      (edge-id (:edges graphedge))
                                new-edge  (update-in edge [:to] conj node-id)
                                new-node  (update-in node [:in] conj edge-id)
                                new-nodes (merge (:nodes graph) (:nodes graphedge) {node-id new-node})
                                new-edges (merge (:edges graph) (:edges graphedge) {edge-id new-edge})]
                            (case next-link-type
                              :edge-link (->GraphEdge edge-id new-nodes new-edges)
                              :node-link (->GraphNode node-id new-nodes new-edges)))))})

(def graph-edge-reactive-graph-impl
  {:get-graphedge identity
   :get-edge (fn
               [graph]
               ((:link-id) (:edges graph)))
   :bind-node-to-source (fn bind-node-to-source
                          ([graph graphnode] (bind-node-to-source graph graphnode :node-link))
                          ([graph graphnode next-link-type]
                           (let [edge-id   (:link-id graph)
                                 edge      (edge-id (:edges graph))
                                 node-id   (:link-id graphnode)
                                 node      (node-id (:nodes graphnode))
                                 new-node  (update-in node [:out]  conj edge-id)
                                 new-edge  (update-in edge [:from] conj node-id)
                                 new-nodes (merge (:nodes graph) (:nodes graphnode) {node-id new-node})
                                 new-edges (merge (:edges graph) (:edges graphnode) {edge-id new-edge})]
                             (case next-link-type
                               :node-link (->GraphNode node-id new-nodes new-edges)
                               :edge-link (->GraphEdge edge-id new-nodes new-edges)))))})

(def default-reactive-node-impl
  {:get-graphnode (fn
                    [node]
                    (let [id (:id node)]
                      (->GraphNode id {id node} {})))})

(def default-reactive-edge-impl
  {:get-graphedge (fn
                    [edge]
                    (let [id (:id edge)]
                      (->GraphEdge id {} {id edge})))})

(extend GraphNode
  TraversableGraph
  default-traversable-graph-impl
  ReactiveNode
  graph-node-reactive-graph-impl)

(extend GraphEdge
  TraversableGraph
  default-traversable-graph-impl
  ReactiveEdge
  graph-edge-reactive-graph-impl)

(extend Node
  PropertyGraph
  default-property-node-impl
  ReactiveNode
  default-reactive-node-impl)

(extend Edge
  PropertyGraph
  default-property-edge-impl
  ReactiveEdge
  default-reactive-edge-impl)

(extend Node-
  PropertyGraph
  default-property-node-impl)

(extend Edge->
  PropertyGraph
  default-property-edge-impl)

;;
;; Builders
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

;; rename to Expression ?
(defrecord Part [tag data])

;; -------------------------
;; Reader Section.
;; =========================

(defprotocol GraphData
  (get-data [elem]))

(extend-type Part
  GraphData
  (get-data [grammar] (:data grammar)))

(extend-type String
  GraphData
  (get-data [txt] txt))

;;
;; Dispatching Stuff
;;

(defn class-map [& elems] (vec (map class elems)))

(defmulti  sym-or-class class)
(defmethod sym-or-class clojure.lang.Symbol [sym] sym)
(defmethod sym-or-class :default [thing] (class thing))
(defn sym-or-class-map [& body] (vec (map sym-or-class body)))

;;
;; Data Collector
;; Todo: refactor safe value methods into a protocol.
;;

(defmulti  attr-safe-value class)
(defmethod attr-safe-value
  clojure.lang.Symbol
  [sym]
  `(get-data ~sym))
(defmethod attr-safe-value
  clojure.lang.Keyword
  [kw]
  kw)
(defmethod attr-safe-value
  java.lang.String
  [txt]
  txt)

(defmulti  label-safe-value class)
(defmethod label-safe-value
  clojure.lang.Symbol
  [sym]
  `(get-data ~sym))
(defmethod label-safe-value
  clojure.lang.Keyword
  [kw]
  kw)

(defn data-collector [[k v]] [k (attr-safe-value v)])

;;
;; Read a List
;;

(defmulti  read-list first)
(defmethod read-list
  'clojure.core/deref
  [[_ name-sym]]
  {:attrs {:name (name name-sym)}})

;;
;; Node Reducer
;;

(defmulti  node-reducer class-map)
(defmethod node-reducer
  [nil clojure.lang.PersistentHashSet]
  [_ hs]
  {:labels (set (map label-safe-value hs))})
(defmethod node-reducer
  [nil clojure.lang.PersistentArrayMap]
  [_ am]
  {:attrs (into {} (map data-collector am))})
(defmethod node-reducer
  [nil clojure.lang.PersistentList]
  [_ ls]
  (read-list ls))
(defmethod node-reducer
  [nil clojure.lang.Symbol]
  [_ elem]
  `(get-data ~elem))
(defmethod node-reducer
  [clojure.lang.PersistentArrayMap clojure.lang.PersistentArrayMap]
  [data am]
  (update-in data [:attrs] merge (into {} (map data-collector am))))

;;
;; Edge Reducer
;;

(defmulti  edge-reducer class-map)
(defmethod edge-reducer
  [nil clojure.lang.Symbol]
  [_ sym]
  `(get-data ~sym))
(defmethod edge-reducer
  [nil clojure.lang.Keyword]
  [_ kw]
  {:labels #{kw}})
(defmethod edge-reducer
  [nil clojure.lang.PersistentHashSet]
  [_ hs]
  {:labels (set (map label-safe-value hs))})

;;
;; Raw Inputs Wrappers
;;

(defmulti  get-node-code class)
(defmethod get-node-code
  clojure.lang.PersistentArrayMap
  [node-params]
  `(make-node ~node-params))
(defmethod get-node-code
  clojure.lang.Cons
  [code]
  code)

(defmulti  get-edge-code class)
(defmethod get-edge-code
  clojure.lang.PersistentArrayMap
  [edge-params]
  `(make-edge ~edge-params))
(defmethod get-edge-code
  clojure.lang.Cons
  [code]
  code)

;;
;; Graph Reducer. TODO: rename to graph-reader.
;;

(defmulti  graph-reducer sym-or-class-map)
(defmethod graph-reducer
  [nil clojure.lang.PersistentList]
  [_ node-body]
  (let [node-data (reduce node-reducer nil node-body)]
    `(get-graphnode ~(get-node-code node-data))))
(defmethod graph-reducer
  [clojure.lang.Cons '-]
  [data _]
  (->Node- data))
(defmethod graph-reducer
  [clojure.lang.Cons '->]
  [data _]
  (->Edge-> data))
(defmethod graph-reducer
  [zigurat.graph.Node- clojure.lang.PersistentVector]
  [node- edge-body]
  (let [edge-data (reduce edge-reducer nil edge-body)
        edge-code `(get-graphedge ~(get-edge-code edge-data))]
    `(bind-node-to-source ~edge-code ~(:code node-) :edge-link)))
(defmethod graph-reducer
  [nil clojure.lang.PersistentVector]
  [_ edge-body]
  (let [edge-data (reduce edge-reducer nil edge-body)]
    `(get-graphedge ~(get-edge-code edge-data))))
(defmethod graph-reducer
  [zigurat.graph.Edge-> clojure.lang.PersistentList]
  [edge-> node-body]
  (let [node-data (reduce node-reducer nil node-body)
        node-code `(get-graphnode ~(get-node-code node-data))]
    `(bind-incoming-edge ~node-code ~(:code edge->))))

(defn read-graph
  [code]
  (reduce graph-reducer nil code))

;; -------------------------
;; End Reader Section.
;; =========================

;;
;; General Data Utils
;;

(defn join-str-data
  [& items]
  (let [items-data (map get-data items)]
     (apply str (clojure.string/join "-" items-data))))
