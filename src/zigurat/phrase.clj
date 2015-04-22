(ns zigurat.phrase
  (:require [zigurat.word]
            [zigurat.punctuation]
            [zigurat.defphrase :refer [defphrase]]
            [zigurat.graph :refer [make-node
                                   make-edge
                                   get-graphnode
                                   get-graphedge
                                   bind-incoming-edge
                                   bind-node-to-source]]))

;;
;; Phrase Level
;;

;; TODO: put into defphrase
(defrecord NP [data])
(defrecord PP [data])
(defrecord QP [data])

;;
;; Grammar methods
;;

(defn top [elem] elem)


(defphrase NP
  ([zigurat.word.JJ zigurat.word.NNS]
   [jj nns]
   (((make-node {:labels #{lbl-jj lbl-nns}})))
   [lbl-jj  (:token jj)
    lbl-nns (:token nns)])
  ([zigurat.word.NNP]
   [nnp]
   (((make-node {:attrs {:name (:token nnp)}}))))
  ([zigurat.phrase.NP zigurat.phrase.PP]
   [np pp]
   (((:data np)) -[(:data pp)]))
  ([zigurat.phrase.NP zigurat.punctuation.COMMA zigurat.phrase.PP]
   [np _ pp]
   (((:data np)) -[(:data pp)]))
  ([zigurat.phrase.QP zigurat.word.NNS]
   [qp nns]
   (((make-node {:labels #{:count} :attrs {:body tok-nns}})) -[(:data qp)])
   [tok-nns (:token nns)]))

(defphrase PP
  ([zigurat.word.IN zigurat.phrase.NP]
   [in np]
   ([(make-edge {:labels #{(:token in)}})]-> ((:data np)))))

(defphrase QP
  ([zigurat.word.RBR zigurat.word.IN zigurat.word.CD]
   [rbr in cd]
   ([(make-edge {:labels #{lbl-rbr lbl-in}})]-> ((make-node {:labels #{:number} :attrs {:body tok-cd}})))
   [lbl-rbr (:token rbr)
    lbl-in  (:token in )
    tok-cd  (:token cd)]))
