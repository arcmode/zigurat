(ns zigurat.core
  "A translator from natural language forms to the cypher query language.

  * A sentence is a list of phrases
  * A phrase is a hashmap with `:phrase` and `:tag` members"

  (:require
   [zigurat.graph   :refer [join-str-data ->Part]]
   [zigurat.grammar :refer [defphrase]]
   [zigurat.nlp     :refer [parse-tree]]))

;; TODO: rename tags to `zigurat.tags/JJ` form.

;;
;; Manual imports. TODO: use some clojure built-in
;; ns-related method if possible.
;;

(defmacro pull [ns vlist]
  `(do ~@(for [i vlist]
           `(def ~i ~(symbol (str ns "/" i))))))

(pull zigurat.graph (get-data nodes edges attrs labels in out))

;;
;; Word Functions: refactor into defword
;;

(defn jj  [token] (->Part :zigurat.grammar/JJ  token))
(defn nns [token] (->Part :zigurat.grammar/NNS token))
(defn in  [token] (->Part :zigurat.grammar/IN  token))
(defn nnp [token] (->Part :zigurat.grammar/NNP token))

(defn rbr [token] (->Part :zigurat.grammar/RBR token))
(defn cd  [token] (->Part :zigurat.grammar/CD  token))

;;
;; Punctutaion Functions
;;

(defn comma     [token] (->Part :zigurat.grammar/COMMA     token))
(defn period    [token] (->Part :zigurat.grammar/PERIOD    token))
(defn colon     [token] (->Part :zigurat.grammar/COLON     token))
(defn semicolon [token] (->Part :zigurat.grammar/SEMICOLON token))
;;
;; Grammar methods
;;

(defn top [elem] elem)

;;    => (let [phrase (np (np (nnp \"Santiago\"))
;;                        (pp (in \"of\")
;;                            (np (nnp \"Chile\"))))]
;;         (-> (match? (phrase get-data)
;;                     '((Santiago) -[:of]-> (Chile)))))
;;       true

(defphrase NP
  "Noun Phrase.

   => (let [phrase (np (jj \"rural\") (nns \"schools\"))]
        (-> phrase get-data nodes first labels))
      #{\"rural\" \"schools\"}

   => (let [phrase (np (nnp \"Santiago\"))]
        (-> phrase get-data nodes first attrs :name))
      \"Santiago\"
"
  ([jj nns]      ((#{jj nns})))
  ([nnp]         (({:name nnp})))
  ([np pp]       ((np) -[pp]))
  ([np comma pp] ((np) -[pp]))
  ([qp nns]      ((#{:count} {:body nns}) -[qp])))

(defphrase PP
  ([in np]       ([#{in}]-> (np))))

(defphrase QP
  ([rbr in cd]   ([#{q}]-> (#{:number} {:body cd}))
   [q (join-str-data rbr in)]))

;; (defphrase VP
;;   ([vbn pp]
;;    ((#{vb}) -)))

;;
;; Parse Tree Evaluation
;;

(def map-eval     (partial map eval))
(def realize-tree (comp map-eval parse-tree))

;; (clojure.pprint/pprint
;;  (macroexpand-1
;;   '(defphrase NP
;;   ([jj nns]
;;    ((#{jj nns})))

;;   ([nnp]
;;    (({:name nnp})))

;;   ([np pp]
;;    ((np) -[pp]))

;;   ([np _ pp]
;;    ((np) -[pp]))

;;   ([qp nns]
;;    ((#{:count} {:body nns}) -[qp])))))

;; (clojure.pprint/pprint
;;  (macroexpand-1
;;   '(defphrase PP
;;   ([in np]
;;    ([#{in}]-> (np))))))


;; (clojure.pprint/pprint
;;  (macroexpand-1
;;   '(defphrase QP
;;   ([rbr in cd]
;;    ([#{q}]-> (#{:number} {:body cd}))
;;    [q (str (:data rbr) "-" (:data in))]))))
