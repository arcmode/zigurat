(ns zigurat.grammar
  "Rename to zigurat.lang || z.defphrase"

  (:require [zigurat.graph
             :refer [->Node-
                     ->Edge->
                     make-node
                     make-edge
                     get-graphnode
                     get-graphedge
                     bind-incoming-edge
                     bind-node-to-source]])
  (:import [zigurat.graph Node- Edge->]))

;;
;; Base Level
;;

(defprotocol GraphData
  (get-data [elem]))

;; rename to Expression ?
(defrecord Part [tag data]
  GraphData
  (get-data [grammar] data))

(extend-type String
  GraphData
  (get-data [txt] txt))


;;
;; General Graph Utils
;;

(defn join-str-data
  [& items]
  (let [items-data (map get-data items)]
     (apply str (clojure.string/join "-" items-data))))


;; ----------------------
;;
;; Defhrase Macro Section
;;
;; ----------------------

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
;;

(defmulti  safe-value class)
(defmethod safe-value
  clojure.lang.Symbol
  [v]
  `(get-data ~v))
(defmethod safe-value
  clojure.lang.Keyword
  [v]
  v)

(defn data-collector [[k v]] [k (safe-value v)])

;;
;; Node Reducer
;;

(defmulti  node-reducer class-map)
(defmethod node-reducer
  [nil clojure.lang.PersistentHashSet]
  [_ hs]
  {:labels (set (map safe-value hs))})
(defmethod node-reducer
  [nil clojure.lang.PersistentArrayMap]
  [_ am]
  {:attrs (into {} (map data-collector am))})
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
  [_ elem]
  `(get-data ~elem))
(defmethod edge-reducer
  [nil clojure.lang.PersistentHashSet]
  [_ hs]
  {:labels (set (map safe-value hs))})

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
;; Graph Reducer
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
    `(bind-node-to-source ~edge-code ~(:code node-))))
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

;;
;; Method Code Factory
;;

(def make-phrase-tag
  (comp (partial keyword "zigurat.grammar") clojure.string/upper-case name))

(defn make-method-header-factory
  [mmname]
  (fn
    [code]
    (concat `(defmethod ~mmname) code)))

(defn make-method-body-factory
  [phrase-tag]
  (fn method-body-factory
    ([args body]
     (method-body-factory args body []))
    ([args body ctx]
     (let [method-body `(->Part ~phrase-tag ~(reduce graph-reducer nil body))
           types (vec (map make-phrase-tag args))]
       (if (seq ctx)
         `(~types
           ~args
           (let ~ctx
             ~method-body))
         `(~types
           ~args
           ~method-body))))))

(defn make-method-factory
  [mm-symbol phrase-tag]
  (comp (make-method-header-factory mm-symbol) (make-method-body-factory phrase-tag)))

(defn make-mdefinitions
  [mm-symbol phrase-tag forms]
  (map (partial apply (make-method-factory mm-symbol phrase-tag)) forms))

;;
;; Defphrase Macro
;;

(defn mm-dispatcher [& elems] (vec (map :tag elems)))

(defmacro defphrase
  [pname & forms]
  (let [phrase-tag    (make-phrase-tag pname)
        mm-symbol     (symbol (clojure.string/lower-case (name pname)))
        m-definitions (make-mdefinitions mm-symbol phrase-tag forms)]
    `(do
       (defmulti ~mm-symbol ~mm-dispatcher)
       ~@m-definitions)))

;; ---------------------------
;;
;; End Defphrase Macro Section
;;
;; ---------------------------

