(ns zigurat.phrase
  (:require [zigurat.word]
            [zigurat.punctuation]
            [zigurat.graph :refer [get-node
                                   make-edge
                                   make-isolated-node
                                   bind-incoming-edge
                                   bind-node-to-source]]))

;;
;; Phrase Level
;;

(defrecord NP [data])
(defrecord PP [data])

(defrecord QP [data])

;;
;; Grammar methods
;;

(defn top [elem] elem)

(defn class-map
  [& elems]
  (vec (map class elems)))

(defmulti np class-map)
(defmulti pp class-map)
(defmulti qp class-map)

(defmethod np
  [zigurat.word.JJ zigurat.word.NNS]
  [jj nns]
  (let [node (make-isolated-node #{(:token jj) (:token nns)} {})]
    (->NP node)))

(defmethod np
  [zigurat.word.NNP]
  [nnp]
  (let [node (make-isolated-node #{} {:name (:token nnp)})]
    (->NP node)))

(defmethod np
  [zigurat.phrase.NP zigurat.phrase.PP]
  [np pp]
  (->NP (bind-node-to-source (:data pp) (:data np))))

(defmethod np
  [zigurat.phrase.NP zigurat.punctuation.COMMA zigurat.phrase.PP]
  [np _ pp]
  (->NP (bind-node-to-source (:data pp) (:data np))))

(defmethod pp
  [zigurat.word.IN zigurat.phrase.NP]
  [in np]
  (let [np-data   (:data np)
        link-node (get-node np-data)
        edge      (make-edge #{(:token in)} {} #{} #{(:id link-node)})
        graph     (bind-incoming-edge np-data edge)]
    (->PP graph)))

(defmethod qp
  [zigurat.word.RBR zigurat.word.IN zigurat.word.CD]
  [rbr in cd]
  (->QP [rbr in cd]))
