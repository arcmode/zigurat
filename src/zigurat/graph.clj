(ns zigurat.graph
  (:require [clojure.set :refer [index union difference]]))

;;
;; Shared Stuff
;;

(defn class-map [& elems] (vec (map class elems)))

;;
;; Graph Protocols
;;

(defprotocol ReactiveNode
  (get-graphnode       [elem])
  (get-node            [elem])
  ;; rename to take-incoming-edge ?
  (add-outgoing-edge   [elem edge]))

(defprotocol ReactiveEdge
  (get-graphedge       [elem])
  (get-edge            [elem])
  ;; rename to take-input-node ?
  (add-source-node     [elem node])
  (add-target-node     [elem node]))

(defprotocol TraversableGraph
  (link  [elem])
  (nodes [elem])
  (edges [elem]))

(defprotocol PropertyGraphElem
  (attrs  [elem])
  (labels [elem])
  (in     [elem])
  (out    [elem])
  (from   [elem])
  (to     [elem]))

;;
;; Semantic Graph Elements
;;

;; TODO: renae link to cursor?
(defrecord GraphEdge [link nodes edges trips index])
(defrecord GraphNode [link nodes edges trips index])
(defrecord Edge      [id labels attrs])
(defrecord Node      [id labels attrs])
;; Transition states (don't know how to implement this feature yet)
(defrecord EdgeCode   [code])
(defrecord EdgeCode-  [code])
(defrecord EdgeCode-> [code])
(defrecord NodeCode   [code])
(defrecord NodeCode-  [code])
(defrecord NodeCode<- [code])

;;
;; Index Utils
;;

(defn unique-index
  "Creates an unique index using set/index.

  ;TODO: implement index and unique-index types with protocols

  => (unique-index #{[:a :b :c]} [1])
     {{1 :b} [:a :b :c]}"
  [data ks]
  (into {} (map #(-> [(first %) (first (second %))]) (index data ks))))

(defn index-elems [elems]
  {:id   (index elems [:id])
   :data (index elems [:labels :attrs])})

(defn index-trips [trips]
  {:source (index        trips [0])
   :edge   (unique-index trips [1])
   :target (index        trips [2])})

(defn indexes
  "Creates an index from sets of nodes, edges and trips.

  ;TODO: implement index type and indexer protocol.

  => (indexes #{{:id 'n1 :labels :ln1 :attrs :an1}
                {:id 'n2 :labels :ln2 :attrs :an2}}
              #{{:id 'e  :labels :le  :attrs :ae}}
              #{['n1 'e 'n2]})
     {:nodes {:id   {{:id 'n1} #{{:id 'n1 :labels :ln1 :attrs :an1}}
                     {:id 'n2} #{{:id 'n2 :labels :ln2 :attrs :an2}}}
              :data {{:labels :ln1 :attrs :an1} #{{:id 'n1 :labels :ln1 :attrs :an1}}
                     {:labels :ln2 :attrs :an2} #{{:id 'n2 :labels :ln2 :attrs :an2}}}}
      :edges {:id   {{:id 'e}  #{{:id 'e  :labels :le  :attrs :ae}}}
              :data {{:labels :le  :attrs :ae}  #{{:id 'e  :labels :le  :attrs :ae}}}}
      :trips {:source {{0 'n1} #{['n1 'e 'n2]}}
              :edge   {{1 'e}    ['n1 'e 'n2]}
              :target {{2 'n2} #{['n1 'e 'n2]}}}}"
  [nodes edges trips]
  {:nodes (index-elems nodes)
   :edges (index-elems edges)
   :trips (index-trips trips)})

;TODO: deflookup
(defn edge-trip [graph edge-id]
  (-> graph :index :trips :edge (get {1 edge-id})))

(defn target-trip [graph node-id]
  (-> graph :index :trips :target (get {2 node-id})))

;;
;; Builders
;;

(defn make-node
  "Create a Node.

  => (class (make-node {:id 'n :labels :l :attrs :a}))
     zigurat.graph.Node"
  [{:keys [id labels attrs]
    :or {id     (gensym "N")
         labels #{}
         attrs   {}}}]
  (->Node id labels attrs))

(defn make-edge
  "Create an Edge.

  => (class (make-edge {:id 'e :labels :l :attrs :a}))
     zigurat.graph.Edge"
  [{:keys [id labels attrs]
    :or {id     (gensym "E")
         labels #{}
         attrs   {}}}]
  (->Edge id labels attrs))

(defn make-graph-node
  [node]
  (let [nodes #{node}
        edges #{}
        trips #{[(:id node) nil nil]}
        index (indexes nodes edges trips)]
    (->GraphNode node nodes edges trips index)))

(defn make-graph-edge
  [edge]
  (let [nodes #{}
        edges #{edge}
        trips #{[nil (:id edge) nil]}
        index (indexes nodes edges trips)]
    (->GraphEdge edge nodes edges trips index)))

;TODO: make a getter-creator fn
;TODO: can be changed to {:nodes :nodes :edges :edges} ?
(def default-traversable-graph-impl
  {:nodes (fn [elem] (:nodes elem))
   :edges (fn [elem] (:edges elem))})

(def graph-node-traversable-graph-impl
  (merge default-traversable-graph-impl
         {:link (fn [graph] (:link graph))}))

(def graph-edge-traversable-graph-impl
  (merge default-traversable-graph-impl
         {:link (fn [graph] (:link graph))}))

(def default-property-graph-impl
  {:attrs  (fn [elem] (:attrs  elem))
   :labels (fn [elem] (:labels elem))})

(defn add-outgoing-edge
  [graphnode graphedge]
   (let [node      (:link graphnode)
         edge      (:link graphedge)
         new-nodes (union (:nodes graphnode) (:nodes graphedge))
         new-edges (union (:edges graphnode) (:edges graphedge))
         edge-id   (:id edge)
         node-id   (:id node)
         ;; query unique index
         old-graphnode-trip [node-id nil nil]
         old-graphedge-trip (edge-trip graphedge edge-id)
         [old-graphedge-trip-source
          _
          old-graphedge-trip-target] old-graphedge-trip

         new-trip  [old-graphedge-trip-source edge-id node-id]
         new-trips (union
                     (difference
                       (:trips graphnode)
                       #{old-graphnode-trip})
                     (difference
                       (:trips graphedge)
                       #{old-graphedge-trip})
                     #{new-trip})
         new-index (indexes new-nodes new-edges new-trips)]
     (->GraphNode node new-nodes new-edges new-trips new-index)))

(def graph-node-reactive-graph-impl
  {:get-graphnode identity
   :get-node (fn [graph] (:link graph))
   ;TODO: refactor into a simple merge and appending of the new trip
   :add-outgoing-edge add-outgoing-edge})

(defn add-source-node
  [graphedge graphnode]
   (let [edge      (:link graphedge)
         node      (:link graphnode)
         new-nodes (union (:nodes graphedge) (:nodes graphnode))
         new-edges (union (:edges graphedge) (:edges graphnode))
         node-id   (:id node)
         edge-id   (:id edge)
         ;; query unique index
         old-graphedge-trip (edge-trip graphedge edge-id)
         old-graphnode-trip [node-id nil nil]
         [old-graphedge-trip-source
          _
          old-graphedge-trip-target] old-graphedge-trip

         new-trip  [node-id edge-id old-graphedge-trip-target]
         new-trips (union
                     (difference
                       (:trips graphedge)
                       #{old-graphedge-trip})
                     (difference
                       (:trips graphnode)
                       #{old-graphnode-trip})
                     #{new-trip})
         new-index (indexes new-nodes new-edges new-trips)]
       (->GraphEdge edge new-nodes new-edges new-trips new-index)))

(def graph-edge-reactive-graph-impl
  {:get-graphedge identity
   :get-edge (fn [graph] (:link graph))
   :add-source-node add-source-node})

(def default-node-reactive-graph-impl
  {:get-graphnode make-graph-node})

(def default-edge-reactive-graph-impl
  {:get-graphedge make-graph-edge})

(extend GraphNode
  TraversableGraph
  graph-node-traversable-graph-impl
  ReactiveNode
  graph-node-reactive-graph-impl)

(extend GraphEdge
  TraversableGraph
  graph-edge-traversable-graph-impl
  ReactiveEdge
  graph-edge-reactive-graph-impl)

(extend Node
  PropertyGraphElem
  default-property-graph-impl
  ReactiveNode
  default-node-reactive-graph-impl)

(extend Edge
  PropertyGraphElem
  default-property-graph-impl
  ReactiveEdge
  default-edge-reactive-graph-impl)

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
  [nil clojure.lang.Keyword]
  [_ kw]
  {:labels #{kw}})
(defmethod node-reducer
  [clojure.lang.PersistentArrayMap clojure.lang.Keyword]
  [data kw]
  (update-in data [:labels] conj kw))
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

(defn read-node [body]
  (reduce node-reducer nil body))

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

(defn read-edge [body]
  (reduce edge-reducer nil body))

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
    (->NodeCode `(get-graphnode ~(get-node-code node-data)))))
(defmethod graph-reducer
  [zigurat.graph.NodeCode '-]
  [code _]
  (->NodeCode- code))
(defmethod graph-reducer
  [zigurat.graph.NodeCode '<-]
  [code _]
  (->NodeCode<- code))
(defmethod graph-reducer
  [zigurat.graph.EdgeCode '-]
  [code _]
  (->EdgeCode- code))
(defmethod graph-reducer
  [zigurat.graph.EdgeCode '->]
  [code _]
  (->EdgeCode-> code))
(defmethod graph-reducer
  [zigurat.graph.EdgeCode- clojure.lang.PersistentList]
  [edge-code- node-body]
  (let [node-data (reduce node-reducer nil node-body)
        node-code `(get-graphnode ~(get-node-code node-data))]
    (->NodeCode `(add-outgoing-edge ~node-code ~(:code edge-code-)))))
(defmethod graph-reducer
  [zigurat.graph.NodeCode- clojure.lang.PersistentVector]
  [node-code- edge-body]
  (let [edge-data (reduce edge-reducer nil edge-body)
        edge-code `(get-graphedge ~(get-edge-code edge-data))]
    (->EdgeCode `(add-source-node ~edge-code ~(:code node-code-)))))
(defmethod graph-reducer
  [zigurat.graph.NodeCode<- clojure.lang.PersistentVector]
  [node-code<- edge-body]
  (let [edge-data (reduce edge-reducer nil edge-body)
        edge-code `(get-graphedge ~(get-edge-code edge-data))]
    (->EdgeCode `(add-target-node ~edge-code ~(:code node-code<-)))))
(defmethod graph-reducer
  [nil clojure.lang.PersistentVector]
  [_ edge-body]
  (let [edge-data (reduce edge-reducer nil edge-body)]
    (->EdgeCode `(get-graphedge ~(get-edge-code edge-data)))))
(defmethod graph-reducer
  [zigurat.graph.EdgeCode-> clojure.lang.PersistentList]
  [edge-> node-body]
  (let [node-data (reduce node-reducer nil node-body)
        node-code `(get-graphnode ~(get-node-code node-data))]
    (->NodeCode `(add-outgoing-edge ~node-code ~(:code edge->)))))

(defn read-graph [code]
  (:code (reduce graph-reducer nil code)))

(defmulti  ltr-to-rtl sym-or-class)
(defmethod ltr-to-rtl clojure.lang.PersistentList
  [node-code]
  node-code)
(defmethod ltr-to-rtl clojure.lang.PersistentVector
  [edge-code]
  edge-code)
(defmethod ltr-to-rtl '-
  [_]
  '-)
(defmethod ltr-to-rtl '->
  [_]
  '<-)

(defn reverse-code [code]
  (rseq (vec (map ltr-to-rtl code))))

(defn read-graph-backward [code-ltr]
  (let [code (reverse-code code-ltr)]
    (read-graph code)))

;; -------------------------
;; End Reader Section.
;; =========================

;;
;; General Data Utils
;;

(defn join-str-data [& items]
  (let [items-data (map get-data items)]
     (apply str (clojure.string/join "-" items-data))))
