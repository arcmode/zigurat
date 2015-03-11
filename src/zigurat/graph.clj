(ns zigurat.graph
  (:require [clojure.set :refer [union]]))

;; [todo] split into graph, edge and node protocols
(defprotocol ReactiveGraph
  "A protocol for reactive graphs."
  (get-node            [elem])
  (get-edge            [elem])
  (bind-node-to-source [elem node])
  (bind-incoming-edge  [elem edge]))

;;
;; Semantic Graph Elements
;;

(defrecord SemanticGraph [link nodes edges]
  ReactiveGraph
  (bind-node-to-source
   [graph node]
   (let [link-id     (second link)
         link-edge   (get-edge (link-id edges))
         new-node-id (:id node)
         new-node    (update-in node      [:out]  conj link-id)
         new-edge    (update-in link-edge [:from] conj new-node-id)
         new-nodes   (assoc nodes new-node-id new-node)
         new-edges   (assoc edges link-id     new-edge)
         new-link    [:nodes new-node-id]]
     (assoc graph :link new-link :nodes new-nodes :edges new-edges)))
  (get-node
   [graph]
   (get-node ((second link) nodes)))
  (bind-incoming-edge
   [graph edge]
   (let [link-id     (second link)
         link-node   (get-node (link-id nodes))
         new-edge-id (:id edge)
         new-edge    (update-in edge      [:to] conj link-id)
         new-node    (update-in link-node [:in] conj new-edge-id)
         new-nodes   (assoc nodes link-id     new-node)
         new-edges   (assoc edges new-edge-id new-edge)
         new-link    [:edges new-edge-id]]
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
         nodes       {id new-node}
         edges       {new-edge-id new-edge}
         link        [:edges new-edge-id]]
     (->SemanticGraph link nodes edges))))

;;
;; helpers
;;

(defn make-isolated-node
  [labels attrs]
  (->SemanticGraphNode (gensym "n") labels attrs #{} #{}))

(defn make-edge
  [labels attrs from to]
  (->SemanticGraphEdge (gensym "e") labels attrs from to))

(defmulti  datum-reader (fn [_ datum] (class datum)))
(defmethod datum-reader
  clojure.lang.Keyword
  [data kw-datum]
  (update-in data [:labels] conj kw-datum))
(defmethod datum-reader
  clojure.lang.PersistentArrayMap
  [data map-datum]
  (update-in data [:attrs]  merge map-datum))
(defmethod datum-reader
  clojure.lang.Symbol
  [data sym-datum]
  (assoc-in data  [:id] sym-datum))

(defn make-node
  [& {:keys [id labels attrs in out]
      :or   {id     (gensym "n")
             labels #{}
             attrs   {}
             in     #{}
             out    #{}}}]
  (->SemanticGraphNode id labels attrs in out))

(defn parse-node
  [& data]
  (let [empty-node (make-node)]
    (reduce datum-reader empty-node data)))

;;
;; Macros
;;

;;
;;(build-path
;;  (:rural :school) -[:in]-> (Santiago :location) -[:of]-> (Chile :location))
;;

(defmulti  dispatch-value class)
(defmethod dispatch-value clojure.lang.Symbol [sym] sym)
(defmethod dispatch-value :default            [other] (class other))

(defmulti  parse-path (fn [& body] (vec (map dispatch-value body))))
(defmethod parse-path
  [clojure.lang.PersistentList]
  [node-body]
  (apply parse-node node-body))
(defmethod parse-path
  [clojure.lang.PersistentList '- clojure.lang.PersistentVector '-> clojure.lang.PersistentList]
  [from-node _ through-edge _ to-node]
  :a-triple)

(defmacro build-path
  [& body]
  `(apply parse-path '~body))

(build-path (Escu :rural :school {:name "Escuelita"}))

(time
 (dotimes [_ 10000]
  (build-path (Escu :rural :school {:name "Escuelita"}))))
