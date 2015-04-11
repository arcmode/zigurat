(ns zigurat.phrase
  (:require [zigurat.word]
            [zigurat.punctuation]
            [zigurat.graph :refer [get-node
                                   make-edge
                                   make-node
                                   make-graph
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
  (let [node (make-node {:labels #{(:token jj) (:token nns)} })]
    (->NP node)))

(defmethod np
  [zigurat.word.NNP]
  [nnp]
  (let [node (make-node {:attrs {:name (:token nnp)}})]
    (->NP node)))

(defmethod np
  [zigurat.phrase.NP zigurat.phrase.PP]
  [np pp]
  (->NP (bind-node-to-source (:data pp) (:data np))))

(defmethod np
  [zigurat.phrase.NP zigurat.punctuation.COMMA zigurat.phrase.PP]
  [np _ pp]
  (->NP (bind-node-to-source (:data pp) (:data np))))

;; (defmethod np
;;   [zigurat.phrase.QP zigurat.word.NNS]
;;   [qp nns]
;;   (let [node-id (gensym "n")
;;         edge-id (gensym "e")
;;         node    (make-node {:id       node-id
;;                             :labels #{:count}
;;                             :attrs   {:collection (:token nns)}
;;                             :out    #{edge-id}})
;;         graph   (:data qp)
;;         link-id (second (:link graph))
;;         edge    (make-edge {:id   edge-id
;;                             :from node-id
;;                             :to   (:id link)})])
;;   (->NP (bind-node-to-source (:data pp) (:data np))))
;;
;; (defmethod qp
;;   [zigurat.word.RBR zigurat.word.IN zigurat.word.CD]
;;   [rbr in cd]
;;   (let [node-rbr-in-id (gensym "n")
;;         node-cd-id     (gensym "n")
;;         edge-id        (gensym "e")
;;         node-rbr-in (make-node
;;                      {:id       node-rbr-in-id
;;                       :labels #{:comparative}
;;                       :attrs   {:body [(:token rbr) (:token in)]}
;;                       :out    #{edge-id}})
;;         edge (make-edge
;;               {:id     edge-id
;;                :from #{node-rbr-in-id}
;;                :to   #{node-cd-id}})
;;         node-cd (make-node
;;                  {:id       node-cd-id
;;                   :labels #{:number}
;;                   :attrs   {:body (:token cd)}
;;                   :in     #{edge-id}})
;;         graph (make-graph
;;                {:link ["nodes" node-rbr-in-id]
;;                 :nodes {node-rbr-in-id node-rbr-in
;;                         node-cd-id     node-cd}
;;                 :edges {edge-id edge}})]
;;     (->QP graph)))

(defmethod pp
  [zigurat.word.IN zigurat.phrase.NP]
  [in np]
  (let [np-data   (:data np)
        link-node (get-node np-data)
        edge      (make-edge {:labels #{(:token in)}
                              :to #{(:id link-node)}})
        graph     (bind-incoming-edge np-data edge)]
    (->PP graph)))




(comment


  (defmethod np
    [zigurat.phrase.QP zigurat.word.NNS]
    [qp nns]
    )

  (defmethod qp
    [zigurat.word.RBR zigurat.word.IN zigurat.word.CD]
    [rbr in cd]
    )

  #(> % 5)

  -[:where]-> (:comparative {:grater-than 5})

  -[:constraint :greather-than]-> (:number {:value 5})

  -|[:greather-than]|-> (:number {:value 5})

  -{:constraint :greather-than}-> (:number {:value 5})




  (pp
   (in "with")
   (np (qp (rbr "more") (in "than") (cd "five")) (nns "students")))

  qp:

  -[:more-than]-> 5

  np:

  (:count {:name "Students"}) -[:more-than]-> 5






)
