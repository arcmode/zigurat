(ns zigurat.core
  "A translator from natural language forms to the cypher query language.

  * A sentence is a list of phrases
  * A phrase is a hashmap with `:phrase` and `:tag` members"

  (:require
   [zigurat.graph   :refer [join-str-data]]
   [zigurat.grammar :refer [defwords defphrase]]
   [zigurat.nlp     :refer [parse-tree]]))

;;
;; Manual imports. TODO: use some clojure built-in
;; ns-related method if possible.
;;

(defmacro pull [ns vlist]
  `(do ~@(for [i vlist]
           `(def ~i ~(symbol (str ns "/" i))))))

(pull zigurat.graph (get-data nodes edges attrs labels in out))

;;
;; Particles
;;

;; Word Functions
(defwords [jj jjr jjs nn nns nnp in rbr cd dt])
;; Punctutaion Functions
(defwords [comma period colon semicolon])

;;
;; Compounds
;;

(defn top [elem] elem)

;;    => (let [phrase (np (np (nnp \"Santiago\"))
;;                        (pp (in \"of\")
;;                            (np (nnp \"Chile\"))))]
;;         (-> (match? (phrase get-data)
;;                     '((@Santiago) -[:of]-> (@Chile)))))
;;       true

(defphrase NP
  "Noun Phrases.

   => (let [phrase (np (jj \"rural\") (nns \"schools\"))]
        (-> phrase get-data nodes first labels))
      #{\"rural\" \"schools\"}

   => (let [phrase (np (nnp \"Santiago\"))]
        (-> phrase get-data nodes first attrs :name))
      \"Santiago\""
  ([jj nns]      ((#{jj nns})))
  ([nnp]         (({:name nnp})))
  ([np pp]       ((np) -[pp]))
  ([np comma pp] ((np) -[pp]))
  ([qp nn]       ((#{:count} {:val nn}) -[qp]))
  ([qp nns]      ((#{:count} {:val nns}) -[qp]))
  ([dt jj nn nn_2] ((#{jj nn nn_2}))))

(defphrase PP
  "Prepositional Phrases.

   => (let [phrase (pp (in \"of\") (np (nnp \"Chile\")))]
        (-> phrase get-data))
      {}"
  ([in np]       ([#{in}]-> (np))))

(defphrase QP
  ([rbr in cd]   ([#{q}]-> (#{:number} {:val cd}))
   [q (join-str-data rbr in)])
  ([jjr in cd]   ([#{q}]-> (#{:number} {:val cd}))
   [q (join-str-data jjr in)])
  ([in jjs cd]   ([#{q}]-> (#{:number} {:val cd}))
   [q (join-str-data in jjs)]))

;;
;; Parse Tree Evaluation
;;

(def map-eval     (partial map eval))
(def realize-tree (comp map-eval parse-tree))

(clojure.pprint/pprint
 (parse-tree ["open conversations with a last agent response"]))

(clojure.pprint/pprint
 (realize-tree ["open conversations with more than three participants"]))

(clojure.pprint/pprint
 (realize-tree ["open conversations with at least one message"]))

(clojure.pprint/pprint
 (realize-tree ["rural schools in Santiago of Chile"]))

(clojure.pprint/pprint
 (realize-tree ["rural schools in Santiago of Chile , with more than eleven courses"]))
