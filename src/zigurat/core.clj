(ns zigurat.core
  "A translator from natural language forms to the cypher query language.

  * A sentence is a list of phrases
  * A phrase is a hashmap with `:phrase` and `:tag` members"

  {:doc/format :markdown}

  (:use opennlp.nlp)
  (:use opennlp.treebank)
  (:use clojure.pprint))

;; Opennlp primitives.

(def ^:private get-sentences (make-sentence-detector "models/en-sent.bin"))
(def ^:private tokenize (make-tokenizer "models/en-token.bin"))
(def ^:private pos-tag (make-pos-tagger "models/en-pos-maxent.bin"))
(def ^:private chunker (make-treebank-chunker "models/en-chunker.bin"))

;; tagging

(defn- tag-sent-seq
  "post-tag a sequence of sentences"
  [sent-seq]
  (map #(-> % tokenize pos-tag chunker) sent-seq))

;; cypher

(defn- cypher-phrase
  "find phrases in the cypher bank"
  [{phrase :phrase}]
  phrase)

(defn- cypher-sent
  "Cypher a sentence.

  1. Find defined phrases
  "
  [sent]
  (map cypher-phrase sent))

(defn- cypher-sent-seq
  "cypher a sequence of sentences"
  [sent-seq]
  (map #(-> % tokenize pos-tag chunker cypher-sent) sent-seq))

;;
;; public
;;

(defn tag-sentences
  "Return pos-tagged chunks from a natural language string with sentences"
  [nl-form]
  (-> nl-form get-sentences tag-sent-seq))

;;
;; TODO: reuse tag-sentences
;;
(defn cypher-sentences
  "Convert a natural language string to cypher:

  1. Pos-tag a natural language form.
  2. Searches for cypher entities.
  3. Guess an optimum representation picking one entity by tag."

  {:doc/format :markdown}

  [nl-form]
  (-> nl-form get-sentences cypher-sent-seq))
