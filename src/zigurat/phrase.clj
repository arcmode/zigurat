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
(defrecord VP [data])

;;
;; Grammar methods
;;

(defn top [elem] elem)


(defphrase NP
  ([zigurat.word.JJ zigurat.word.NNS]
   [jj nns]
   ((#{jj-token nns-token}))
   [jj-token  (:token jj)
    nns-token (:token nns)])

  ([zigurat.word.NNP]
   [nnp]
   (({:name nnp-token}))
   [nnp-token (:token nnp)])

  ([zigurat.phrase.NP zigurat.phrase.PP]
   [np pp]
   ((np-data) -[pp-data])
   [np-data (:data np)
    pp-data (:data pp)])

  ([zigurat.phrase.NP zigurat.punctuation.COMMA zigurat.phrase.PP]
   [np _ pp]
   ((np-data) -[pp-data])
   [np-data (:data np)
    pp-data (:data pp)])

  ([zigurat.phrase.QP zigurat.word.NNS]
   [qp nns]
   ((#{:count} {:body nns-token}) -[qp-data])
   [nns-token (:token nns)
    qp-data   (:data qp)]))

(defphrase PP
  ([zigurat.word.IN zigurat.phrase.NP]
   [in np]
   ([#{in-token}]-> (np-data))
   [in-token (:token in)
    np-data  (:data np)]))

(defphrase QP
  ([zigurat.word.RBR zigurat.word.IN zigurat.word.CD]
   [rbr in cd]
   ([#{lbl-rbr lbl-in}]-> (#{:number} {:body tok-cd}))
   [lbl-rbr (:token rbr)
    lbl-in  (:token in)
    tok-cd  (:token cd)]))

;; (defphrase VP
;;   ([zigurat.word.VP zigurat.phrase.S]
;;    [vp s]
;;    ((#{vb-tok}) -)))
