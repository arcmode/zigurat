(ns zigurat.word)

;;
;; Word Level
;;

(defrecord JJ  [token])
(defrecord NNS [token])
(defrecord IN  [token])
(defrecord NNP [token])

;;
;; helpers
;;

(defn jj  [word] (->JJ  word))
(defn nns [word] (->NNS word))
(defn in  [word] (->IN  word))
(defn nnp [word] (->NNP word))
