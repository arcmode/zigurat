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
  (add-outgoing-edge   [elem edge])
  (add-incoming-edge   [elem edge]))

(defprotocol ReactiveEdge
  (get-graphedge       [elem])
  (get-edge            [elem])
  (add-source-node     [elem node])
  (add-target-node     [elem node]))

(defprotocol ReactiveGraph
  (obs-trips-as-src [graph])
  (obs-trips-as-tgt [graph])
  (new-trip-as-src  [graph] [graph base-trip])
  (new-trip-as-tgt  [graph] [graph base-trip]))

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

(def not-nil? (complement nil?))

(defn trip-elem-picker [a b]
  (let [both-non-nil (and (not-nil? a) (not-nil? b))]
    (if both-non-nil
      (throw (Exception. "Cannot pick a value between two non nil values."))
      (or a b))))

(defn assemble-trip [src-trip tgt-trip]
  (let [src-id  (trip-elem-picker (get src-trip 0) (get tgt-trip 0))
        edge-id (trip-elem-picker (get src-trip 1) (get tgt-trip 1))
        tgt-id  (trip-elem-picker (get src-trip 2) (get tgt-trip 2))]
    [src-id edge-id tgt-id]))

(defn connect-graphs
  [source target]
  (let [new-nodes     (union (:nodes source) (:nodes target))
        new-edges     (union (:edges source) (:edges target))
        ;; get obsolete trips
        obs-src-trips (obs-trips-as-src source)
        obs-tgt-trips (obs-trips-as-tgt target)
        new-trip      (assemble-trip (new-trip-as-src source)
                                     (new-trip-as-tgt target))
        new-trips     (union
                       (difference (:trips source) obs-src-trips)
                       (difference (:trips target) obs-tgt-trips)
                       #{new-trip})
        new-index     (indexes new-nodes new-edges new-trips)]
    (merge source {:nodes new-nodes :edges new-edges
                   :trips new-trips :index new-index})))

(defn add-outgoing-edge [graphnode graphedge]
  (connect-graphs graphnode graphedge))

(defn add-source-node [graphedge graphnode]
  (connect-graphs graphnode graphedge))

(defn add-target-node [graphedge graphnode]
  (connect-graphs graphedge graphnode))

(def graph-node-reactive-node-impl
  {:get-graphnode     identity
   :get-node          (fn [graph] (:link graph))
   :add-outgoing-edge add-outgoing-edge})

(def graph-edge-reactive-edge-impl
  {:get-graphedge   identity
   :get-edge        (fn [graph] (:link graph))
   :add-source-node add-source-node
   :add-target-node add-target-node})

(def node-reactive-node-impl
  {:get-graphnode make-graph-node})

(def edge-reactive-edge-impl
  {:get-graphedge make-graph-edge})

;; TODO: DRY
(defn graph-node-obs-trips-as-src [src]
  #{[(-> src :link :id) nil nil]})
(defn graph-node-obs-trips-as-tgt [tgt]
  #{[(-> tgt :link :id) nil nil]})

(defn graph-node-new-trip-as-src [src]
  [(-> src :link :id) nil nil])
(defn graph-node-new-trip-as-tgt [tgt]
  [nil nil (-> tgt :link :id)])

;; TODO: DRY
(defn graph-edge-obs-trips-as-src [src]
  #{(edge-trip src (-> src :link :id))})
(defn graph-edge-obs-trips-as-tgt [tgt]
  #{(edge-trip tgt (-> tgt :link :id))})

;; TODO: edge-trip-src
(defn graph-edge-new-trip-as-src [src]
  (let [link-id (-> src :link :id)
        src-id  (-> (edge-trip src link-id) (get 0))]
    [src-id link-id nil]))
(defn graph-edge-new-trip-as-tgt [tgt]
  (let [link-id (-> tgt :link :id)
        tgt-id  (-> (edge-trip tgt link-id) (get 2))]
    [nil link-id tgt-id]))

(def graph-node-reactive-graph-impl
  {:obs-trips-as-src graph-node-obs-trips-as-src
   :obs-trips-as-tgt graph-node-obs-trips-as-tgt
   :new-trip-as-src  graph-node-new-trip-as-src
   :new-trip-as-tgt  graph-node-new-trip-as-tgt})

(def graph-edge-reactive-graph-impl
  {:obs-trips-as-src graph-edge-obs-trips-as-src
   :obs-trips-as-tgt graph-edge-obs-trips-as-tgt
   :new-trip-as-src  graph-edge-new-trip-as-src
   :new-trip-as-tgt  graph-edge-new-trip-as-tgt})

(extend GraphNode
  TraversableGraph graph-node-traversable-graph-impl
  ReactiveNode     graph-node-reactive-node-impl
  ReactiveGraph    graph-node-reactive-graph-impl)

(extend GraphEdge
  TraversableGraph graph-edge-traversable-graph-impl
  ReactiveEdge     graph-edge-reactive-edge-impl
  ReactiveGraph    graph-edge-reactive-graph-impl)

(extend Node
  PropertyGraphElem default-property-graph-impl
  ReactiveNode      node-reactive-node-impl)

(extend Edge
  PropertyGraphElem default-property-graph-impl
  ReactiveEdge      edge-reactive-edge-impl)

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
  (->NodeCode- (:code code)))
(defmethod graph-reducer
  [zigurat.graph.NodeCode '<-]
  [code _]
  (->NodeCode<- (:code code)))
(defmethod graph-reducer
  [zigurat.graph.EdgeCode '-]
  [code _]
  (->EdgeCode- (:code code)))
(defmethod graph-reducer
  [zigurat.graph.EdgeCode '->]
  [code _]
  (->EdgeCode-> (:code code)))
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
