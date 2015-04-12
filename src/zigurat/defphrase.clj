(ns zigurat.defphrase
  (:require [zigurat.graph :refer :all]))

;;
;; Transition states (don't know (where to put/how to call) this feature yet)
;;

(defrecord Node-  [data])
(defrecord Edge-> [data])

;;
;; General purpose dispatcher fns
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
  [nil clojure.lang.Keyword]
  [_ kw]
  {:labels #{kw}})
(defmethod node-reducer
  [clojure.lang.PersistentArrayMap clojure.lang.Keyword]
  [node-data kw]
  (update-in node-data [:labels] clojure.set/union #{kw}))
(defmethod node-reducer
  [nil clojure.lang.Symbol]
  [_ sym]
  sym)
(defmethod node-reducer
  [nil clojure.lang.PersistentArrayMap]
  [_ node-data]
  node-data)
(defmethod node-reducer
  [nil clojure.lang.PersistentList]
  [_ code]
  code)

;;
;; Edge Reducer
;;

(defmulti  edge-reducer class-map)
(defmethod edge-reducer
  [nil clojure.lang.Keyword]
  [_ kw]
  {:labels #{kw}})
(defmethod edge-reducer
  [nil clojure.lang.PersistentList]
  [_ code]
  code)

;;
;; Body Reducer (should I rename this to Graph Reducer?)
;;

(defmulti  body-reducer sym-or-class-map)
(defmethod body-reducer
  [nil clojure.lang.PersistentList]
  [_ node-data]
  `(get-graphnode ~(reduce node-reducer nil node-data)))
(defmethod body-reducer
  [clojure.lang.Cons '-]
  [data _]
  (->Node- data))
(defmethod body-reducer
  [clojure.lang.Cons '->]
  [data _]
  (->Edge-> data))
(defmethod body-reducer
  [zigurat.defphrase.Node- clojure.lang.PersistentVector]
  [node- edge-data]
  (let [edge-code `(get-graphedge ~(reduce edge-reducer nil edge-data))]
    `(bind-node-to-source ~edge-code ~(:data node-))))
(defmethod body-reducer
  [nil clojure.lang.PersistentVector]
  [_ edge-data]
  `(get-graphedge ~(reduce edge-reducer edge-data)))
(defmethod body-reducer
  [zigurat.defphrase.Edge-> clojure.lang.PersistentList]
  [edge-> node-data]
  (let [node-code `(get-graphnode ~(reduce node-reducer nil node-data))]
    `(bind-incoming-edge ~node-code ~(:data edge->))))

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
     (let [method-body `(~constr-symbol ~(reduce body-reducer nil body))]
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
