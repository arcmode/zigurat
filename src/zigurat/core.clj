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

(def pos-tags (hash-set
              :CC	    ;coordinating conjunction	              and
              :CD	    ;cardinal number	                      1, third
              :DT	    ;determiner	                            the
              :EX	    ;existential there	                    there is
              :FW	    ;foreign word	                          d’hoevre
              :IN	    ;preposition/subordinating conjunction	in, of, like
              :JJ	    ;adjective	                            big
              :JJR    ;adjective, comparative	                bigger
              :JJS    ;adjective, superlative	                biggest
              :LS	    ;list marker	                          1)
              :MD	    ;modal	could,                          will
              :NN	    ;noun, singular or mass	                door
              :NNS	  ;noun plural	                          doors
              :NNP	  ;proper noun, singular	                John
              :NNPS   ;proper noun, plural	                  Vikings
              :PDT	  ;predeterminer	                        both the boys
              :POS	  ;possessive ending                      friend‘s
              :PRP	  ;personal pronoun                       I, he, it
              :PRP$   ;possessive pronoun                     my, his
              :RB	    ;adverb	                                however, usually, naturally, here, good
              :RBR	  ;adverb, comparative                    better
              :RBS	  ;adverb, superlative                    best
              :RP	    ;particle                               give up
              :TO	    ;to                                     to go, to him
              :UH	    ;interjection                           uhhuhhuhh
              :VB	    ;verb, base form	                      take
              :VBD	  ;verb, past tense	                      took
              :VBG	  ;verb, gerund/present participle	      taking
              :VBN	  ;verb, past participle	                taken
              :VBP	  ;verb, sing. present, non-3d	          take
              :VBZ	  ;verb, 3rd person sing. present	        takes
              :WDT	  ;wh-determiner	                        which
              :WP	    ;wh-pronoun	                            who, what
              :WP$    ;possessive                             wh-pronoun	whose
              :WRB	  ;wh-abverb	                            where, when
              ))

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
                                 I fight for the Users!")))
           (assert (= '((["Awarded"    "JJ"]
                         ["architects" "NNS"]
                         ["in"         "IN"]
                         ["Santiago"   "NNP"]
                         ["of"         "IN"]
                         ["Chile"      "NNP"]
                         ["."          "."])
                        (["Rural"      "NNP"]
                         ["schools"    "NNS"]
                         ["with"       "IN"]
                         ["more"       "JJR"]
                         ["than"       "IN"]
                         ["fifty"      "CD"]
                         ["student"    "NN"]
                         ["."          "."]))
                      (sentences "Awarded architects in Santiago of Chile.
                                 Rural schools with more than fifty student."))))
   }
  [text]
  (let [reader-func (comp (partial map (comp chunker
                                             pos-tag
                                             tokenize))
                          get-sentences)]
    (reader-func text)))

; transforming

;; ((["Rural"      "NNP"]     ->
;;   ["schools"    "NNS"]
;;   ["in"         "IN"]
;;   ["Santiago"   "NNP"]
;;   ["of"         "IN"]
;;   ["Chile"      "NNP"]
;;   ["."          "."]))

;; phrase hooks for {:phrase ["Rural" "schools"], :tag "NP"}
;; "NP_rural_schools"
;; "HEAD_NP_rural"
;;

(sentences "Rural in Santiago of Chile.")

'(({:phrase ["Rural" "schools"], :tag "NP"}
  {:phrase ["in"], :tag "PP"}
  {:phrase ["Santiago"], :tag "NP"}
  {:phrase ["of"], :tag "PP"}
  {:phrase ["Chile"], :tag "NP"}))


;; (([{:type "edge"
;;     :labels ["awarded"]
;;     :tag "JJ"},
;;    {:type "node"
;;     :labels ["architects"]
;;     :tag "NNS"},
;;    {:type: "edge"
;;     :labels ["in"]
;;     :tag "IN"},
;;    {:type "node"
;;     :name "Santiago"
;;     :tag "NNS"},
;;    {:type: "edge"
;;     :labels ["of"]
;;     :tag "IN"},
;;    {:type "node"
;;     :name "Chile"
;;     :tag "NNS"}]))

;; for-each-word
;; (sen-ql :rural :school -in-> @santiago:location -in-> @chile:location)
;; (sen-ql :rural :location <-in+- :school -in-> @santiago:location -in-> @chile:location)

(sentences "Rural schools")

(sentences "Malloco is a beautiful rural location in Central Chile.")


; utils

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
