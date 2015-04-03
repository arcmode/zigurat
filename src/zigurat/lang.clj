(ns zigurat.lang)

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

(defn make-edge
  [& {:keys [id labels attrs from to]
      :or   {id     (gensym "e")
             labels #{}
             attrs   {}
             from   #{}
             to     #{}}}]
  (->SemanticGraphEdge id labels attrs from to))

(defn make-graph
  [& {:keys [link nodes edges]
      :or   {link   nil
             nodes  {}
             edges  {}}}]
  (->SemanticGraph link nodes edges))

(defn parse-node
  [& data]
  (let [empty-node (make-node)]
    (reduce datum-reader empty-node data)))

(defn parse-edge
  [& data]
  (let [empty-edge (make-edge)]
    (reduce datum-reader empty-edge data)))

;;
;; Macros
;;


(defmulti  parse-entity class)

(defmethod parse-entity
  clojure.lang.PersistentList
  [node-body]
  (apply parse-node node-body))
(defmethod parse-entity
  clojure.lang.PersistentVector
  [edge-body]
  (apply parse-edge edge-body))
(defmethod parse-entity
  clojure.lang.Symbol
  [sym]
  (eval sym))

(defmulti read-graph-symbol (fn [sym] (contains? #{'- '->} sym)))
(defmethod read-graph-symbol
  true
  [sym]
  sym)
(defmethod read-graph-symbol
  false
  [sym]
  :eval)

(defmulti  dispatch-value class)
(defmethod dispatch-value clojure.lang.Symbol [sym] (read-graph-symbol sym))
(defmethod dispatch-value :default            [other] (class other))

(defmulti  parse-path (fn [& body] (vec (map dispatch-value body))))

(defmethod parse-path
  [zigurat.graph.SemanticGraphNode]
  [node]
   node)

(defmethod parse-path
  ['- clojure.lang.PersistentVector '-> zigurat.graph.SemanticGraphNode]
  [_ edge-body _ node]
  (let [edge (apply parse-edge edge-body)]
    (bind-incoming-edge node edge)))
(defmethod parse-path
  ['- clojure.lang.PersistentVector '-> zigurat.graph.SemanticGraph]
  [_ edge-body _ graph]
  (let [edge (apply parse-edge edge-body)]
    (bind-incoming-edge graph edge)))
(defmethod parse-path
  [zigurat.graph.SemanticGraphNode '- zigurat.graph.SemanticGraph]
  [from _ through-to]
  (bind-node-to-source through-to from))

(defn parse-symbol
  [sym]
  (if (contains? #{'- '->} sym)
    sym
    (parse-entity sym)))

(defmacro build-path
  [& body]
  `(apply parse-path (map parse-symbol '~body)))

(macroexpand-1 '(build-path (:rural)))


(parse-path '- [:in] '-> (parse-path '(:location {:name "Chile"})))

;; (build-path

;;   (sch :rural :school)
;;   -
;;   [:in]
;;   ->
;;   (stgo :location {:name "Santiago"})
;;   -
;;   [:in]
;;   ->
;;   (stgo :location {:name "Chile"})

;; )






;;(time
;; (dotimes [_ 10000]
;;  (build-path  -[:in]-> (chi :location {:name "Chile"}))))
