(ns zigurat.punctuation)

;;
;; Punctutaion Level
;;

(defrecord COMMA     [token])
(defrecord PERIOD    [token])
(defrecord COLON     [token])
(defrecord SEMICOLON [token])

;;
;; helpers
;;

(defn comma     [token] (->COMMA     token))
(defn period    [token] (->PERIOD    token))
(defn colon     [token] (->COLON     token))
(defn semicolon [token] (->SEMICOLON token))
