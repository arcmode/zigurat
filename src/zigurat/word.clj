(ns zigurat.word)

;;
;; Word Level
;;

(defrecord JJ  [token])
(defrecord NNS [token])
(defrecord IN  [token])
(defrecord NNP [token])

(defrecord RBR [token])
(defrecord CD  [token])

;;
;; helpers
;;

(defn jj  [word] (->JJ  word))
(defn nns [word] (->NNS word))
(defn in  [word] (->IN  word))
(defn nnp [word] (->NNP word))

(defn rbr [word] (->RBR word))
(defn cd  [word] (->CD  word))
