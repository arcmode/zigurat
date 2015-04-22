(ns zigurat.core
  "A translator from natural language forms to the cypher query language.

  * A sentence is a list of phrases
  * A phrase is a hashmap with `:phrase` and `:tag` members"

  (:require [zigurat.nlp]
            [zigurat.word]
            [zigurat.phrase]
            [zigurat.punctuation]))

(def map-eval     (partial map eval))
(def realize-tree (comp map-eval zigurat.nlp/parse-tree))

(defmacro pull [ns vlist]
  `(do ~@(for [i vlist]
           `(def ~i ~(symbol (str ns "/" i))))))

(pull zigurat.word        (nns nnp jj in rbr cd))
(pull zigurat.phrase      (top np pp qp))
(pull zigurat.punctuation (comma period colon semicolon))
