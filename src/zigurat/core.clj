(ns zigurat.core
  "A translator from natural language forms to the cypher query language.

  * A sentence is a list of phrases
  * A phrase is a hashmap with `:phrase` and `:tag` members"

  (:require [zigurat.grammar  :refer :all]
            [opennlp.treebank :refer [make-treebank-parser]]
            [clojure.string   :refer [lower-case]]))

;;
;; Word Functions
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

(defphrase NP
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


;; OpenNLP
;;

(def punctuation-map
  {\, "comma"
   \. "period"
   \: "colon"
   \; "semicolon"})

(defmacro if-apply
  [subject test? then else]
  `(if (~test? ~subject) (~then ~subject) (~else ~subject)))

(def treebank-parser (make-treebank-parser "models/en-parser-chunking.bin"))

(declare ^:private read-node)

(def name-node (comp (partial symbol "zigurat.core") lower-case name))

(defn read-tree
  [[node & children]]
  (lazy-seq (conj (map read-node children) (name-node node))))

(defn- read-node
  [node]
  (if-apply node seq? read-tree name))

(defn parse-string
  [string]
  (apply str (replace punctuation-map string)))

(def str->tree (comp read-tree read-string parse-string))

(def make-trees (partial map str->tree))

(def parse-tree (comp make-trees treebank-parser))

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
