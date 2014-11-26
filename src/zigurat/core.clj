(ns zigurat.core
  "A translator from natural language forms to the cypher query language.

  * A sentence is a list of phrases
  * A phrase is a hashmap with `:phrase` and `:tag` members"

  {:doc/format :markdown}

  (:require [opennlp.nlp      :refer [make-sentence-detector
                                      make-tokenizer
                                      make-pos-tagger]]
            [opennlp.treebank :refer [make-treebank-chunker]]
            [inflections.core :refer :all]
            [clojure.string   :refer (join)]))

;;
;; An atom to store the mapping from nlp domain to our data model
;; It is `schema` the correct word?
;;
(def ^:dynamic *schema* (atom (read-string (slurp "schema/default-en.edn"))))


(let [test-str "school"
      test-reg (re-pattern (str "(?i)" (singular test-str) "|" (plural test-str)))]
  (boolean (re-matches test-reg "Schools")))


;; OpenNLP tools

(def ^:private get-sentences (make-sentence-detector "models/en-sent.bin"))
(def ^:private tokenize (make-tokenizer "models/en-token.bin"))
(def ^:private pos-tag (make-pos-tagger "models/en-pos-maxent.bin"))
(def ^:private chunker (make-treebank-chunker "models/en-chunker.bin"))


;; tagging

(defn sentences
  {:doc "Applies `tokenize` and `postag` to each sentence returned by `get-sentences`"
   :test (fn []
           (assert (= '((["[last"   "JJ"]
                         ["words"   "NNS"]
                         [","       ","]
                         ["turning" "VBG"]
                         ["on"      "IN"]
                         ["Clu"     "NNP"]
                         ["during"  "IN"]
                         ["an"      "DT"]
                         ["air"     "NN"]
                         ["battle"  "NN"]
                         ["]"       "IN"]
                         ["I"       "PRP"]
                         ["fight"   "VBP"]
                         ["for"     "IN"]
                         ["the"     "DT"]
                         ["Users"   "NNS"]
                         ["!"       "."]))
                      (sentences "[last words, turning on Clu during an air battle]
                                 I fight for the Users!"))))
   }
  [text]
  (let [reader-func (comp (partial map (comp pos-tag
                                             tokenize))
                          get-sentences)]
    (reader-func text)))


(defn sym-list-to-string
  {:doc "Returns the joined string representation of the symbols in sym-list"
   :test (fn []
           (assert (= "will not eval"
                      (sym-list-to-string '(will not eval)))))}
  [sym-list]
  (join " " (map str sym-list)))


(defmacro read-raw
  {:doc "Applies form-to-strings and ~method to the ~payload."
   :test (fn []
           (assert (= '(clojure.core/-> (quote (will not eval))
                                        zigurat.core/sym-list-to-string
                                        identity)
                       (macroexpand-1 '(read-raw identity will not eval)))))}
  [method & payload]
  `(-> '~payload sym-list-to-string ~method))

