(ns zigurat.nlp
  (:require [opennlp.nlp        :refer [make-sentence-detector
                                        make-tokenizer
                                        make-pos-tagger]]
            [opennlp.tools.lazy :refer [lazy-chunk]]
            [opennlp.treebank   :refer [make-treebank-chunker]]))

;; OpenNLP tools

(def get-sentences (make-sentence-detector "models/en-sent.bin"))
(def tokenize (make-tokenizer "models/en-token.bin"))
(def pos-tag (make-pos-tagger "models/en-pos-maxent.bin"))
(def chunker (make-treebank-chunker "models/en-chunker.bin"))

;; tagging

(defn read-raw
  {:doc "Applies `tokenize` and `postag` to each sentence returned by `get-sentences`"
   :test (fn []
           (assert (= '(({:phrase ["[last" "words" ","], :tag "NP"}
                         {:phrase ["turning"], :tag "VP"}
                         {:phrase ["on"], :tag "PP"}
                         {:phrase ["Clu"], :tag "NP"}
                         {:phrase ["during"], :tag "PP"}
                         {:phrase ["an" "air" "battle"], :tag "NP"}
                         {:phrase ["]"], :tag "PP"}
                         {:phrase ["I"], :tag "NP"}
                         {:phrase ["fight"], :tag "VP"}
                         {:phrase ["for"], :tag "PP"}
                         {:phrase ["the" "Users"], :tag "NP"}))
                      (read-raw ["[last words, turning on Clu during an air battle]
                                 I fight for the Users!"]))))}
  [text]
  (lazy-chunk text tokenize pos-tag chunker))
