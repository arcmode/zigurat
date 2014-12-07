(ns zigurat.core
  "A translator from natural language forms to the cypher query language.

  * A sentence is a list of phrases
  * A phrase is a hashmap with `:phrase` and `:tag` members"

  {:doc/format :markdown}

  (:require [opennlp.nlp        :refer [make-sentence-detector
                                        make-tokenizer
                                        make-pos-tagger]]
            [opennlp.treebank   :refer [make-treebank-chunker]]
            [opennlp.tools.lazy :refer [lazy-chunk]]
            [inflections.core   :refer :all]
            [clojure.string     :refer (join)]))

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

(defn read-raw
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
                      (read-raw "[last words, turning on Clu during an air battle]
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
                      (read-raw "Awarded architects in Santiago of Chile.
                               Rural schools with more than fifty student."))))
   }
  [text]
  (lazy-chunk text tokenize pos-tag chunker))

(defn resolve-token
  [token]
  (if (Character/isUpperCase (first token))
    {:name token}
    (singular (keyword token))))

(defn make-token-mapper
  [phrase-tag]
  (fn [token]
    (let [token-key (keyword token)
          tag-key (keyword phrase-tag)]
      (if-let [g-data (tag-key (token-key @*schema*))]
        g-data
        (resolve-token token)))))

(defn map-phrase
  [phrase]
  (let [map-token (make-token-mapper (:tag phrase))
        tokens (:phrase phrase)]
    (map map-token tokens)))

(defn map-sentence
  [phrases]
  (map map-phrase phrases))

(defn map-sentences
  [sentences]
  (map map-sentence sentences))

(-> ["rural schools in Santiago of Chile. Yes." "architects nearby."]
    read-raw
    map-sentences
    first)

(-> ["rural schools in Santiago of Chile. Yes." "rural schools in Santiago of Chile. Yes."]
    read-raw
    map-sentences
    )

"MATCH (a:Rural:School) -[:IN*]-> (b {name: \"Santiago\"}) -[:IN*]-> (c {name: \"Chile\"})"
