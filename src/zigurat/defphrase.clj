(ns zigurat.defphrase
  (:require [zigurat.graph :refer :all]))

;;
;; Transition states (don't know (where to put/how to call) this feature yet)
;;

(defrecord Node-  [code])
(defrecord Edge-> [code])

;;
;; Dispatching stuff
;;

(defn class-map
  [& elems]
  (vec (map class elems)))

(defmulti  sym-or-class class)
(defmethod sym-or-class clojure.lang.Symbol [sym] sym)
(defmethod sym-or-class :default [thing] (class thing))
(defn sym-or-class-map [& body] (vec (map sym-or-class body)))

;;
;; Node Reducer
;;

(defmulti  node-reducer class-map)
(defmethod node-reducer
  [nil clojure.lang.PersistentHashSet]
  [_ labels]
  {:labels labels})
(defmethod node-reducer
  [nil clojure.lang.PersistentArrayMap]
  [_ attrs]
  {:attrs attrs})
(defmethod node-reducer
  [nil clojure.lang.Symbol]
  [_ elem]
  elem)
(defmethod node-reducer
  [clojure.lang.PersistentArrayMap clojure.lang.PersistentArrayMap]
  [data attrs]
  (update-in data [:attrs] merge attrs))

;;
;; Edge Reducer
;;

(defmulti  edge-reducer class-map)
(defmethod edge-reducer
  [nil clojure.lang.Symbol]
  [_ elem]
  elem)
(defmethod edge-reducer
  [nil clojure.lang.PersistentHashSet]
  [_ labels]
  {:labels labels})

;;
;; Raw inputs wrappers
;;

(defmulti  get-node-code class)
(defmethod get-node-code
  clojure.lang.PersistentArrayMap
  [node-params]
  `(make-node ~node-params))
(defmethod get-node-code
  clojure.lang.Symbol
  [sym]
  sym)

(defmulti  get-edge-code class)
(defmethod get-edge-code
  clojure.lang.PersistentArrayMap
  [edge-params]
  `(make-edge ~edge-params))
(defmethod get-edge-code
  clojure.lang.Symbol
  [sym]
  sym)

;;
;; Body Reducer (should I rename this to Graph Reducer?)
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
  [zigurat.defphrase.Node- clojure.lang.PersistentVector]
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
  [zigurat.defphrase.Edge-> clojure.lang.PersistentList]
  [edge-> node-body]
  (let [node-data (reduce node-reducer nil node-body)
        node-code `(get-graphnode ~(get-node-code node-data))]
    `(bind-incoming-edge ~node-code ~(:code edge->))))

;;
;; Method Factory
;;

(defn make-method-header-factory
  [mmname]
  (fn
    [code]
    (concat `(defmethod ~mmname) code)))

(defn make-method-body-factory
  [constr-symbol]
  (fn method-body-factory
    ([types args body]
     (method-body-factory types args body []))
    ([types args body ctx]
     (let [method-body `(~constr-symbol ~(reduce graph-reducer nil body))]
       (if (seq ctx)
         `(~types
           ~args
           (let ~ctx
             ~method-body))
         `(~types
           ~args
           ~method-body))))))

(defn make-method-factory
  [mm-symbol constr-symbol]
  (comp (make-method-header-factory mm-symbol) (make-method-body-factory constr-symbol)))

(defn make-mdefinitions
  [mm-symbol constr-symbol forms]
  (map (partial apply (make-method-factory mm-symbol constr-symbol)) forms))

;;
;; Defphrase Macro
;;

(defn mmdispatcher [& elems] (vec (map class elems)))

(defmacro defphrase
  [pname & forms]
  (let [mm-name       (name pname)
        mm-symbol     (symbol (clojure.string/lower-case mm-name))
        constr-symbol (symbol (str "->" (clojure.string/upper-case mm-name)))
        mdefinitions (make-mdefinitions mm-symbol constr-symbol forms)]
    `(do
       (defmulti ~mm-symbol ~mmdispatcher)
       ~@mdefinitions)))
