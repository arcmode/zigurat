(ns zigurat.nlp
  "NLP tools like a `tree-parser`"

  (:require [opennlp.treebank :refer [make-treebank-parser]]
            [clojure.string   :refer [lower-case]]))

(def punctuation-map
  {\, "comma"
   \. "period"
   \: "colon"
   \; "semicolon"})

(defmacro if-apply
  "A simple macro for dispatching between two functions.

  => (macroexpand-1 '(zigurat.nlp/if-apply 1 number? inc identity))
     '(if (number? 1) (inc 1) (identity 1))"
  [subject test? then else]
  `(if (~test? ~subject) (~then ~subject) (~else ~subject)))

;;
;; OpenNLP
;;

(def treebank-parser (make-treebank-parser "models/en-parser-chunking.bin"))

(declare read-node)

(def name-node (comp (partial symbol "zigurat.core") lower-case name))

(defn read-tree
  "Converts unqualified expressions into zigurat-qualified expressions.

  => (read-tree '(np (jj rural) (nns schools)))
     '(zigurat.core/np
        (zigurat.core/jj \"rural\")
        (zigurat.core/nns \"schools\"))"
  [[node & children]]
  (lazy-seq (conj (map read-node children) (name-node node))))

(defn read-node
  "Reads a node taken from an s-expr as it's zigurat-qualified equivalent.

  => (read-node 'rural)
     \"rural\"

  => (read-node '(jj rural))
     (read-tree '(jj rural))"
  [node]
  (if-apply node seq? read-tree name))

(defn parse-string
  "Replaces invalid symbols related to punctuation.

  => (parse-string \". , : ;\")
     \"period comma colon semicolon\""
  [string]
  (apply str (replace punctuation-map string)))

(def str->tree
  "Reads a string into a zigurat-qualified nlp expression.

  => (str->tree \"(np (jj rural) (nns schools))\")
     '(zigurat.core/np
        (zigurat.core/jj \"rural\")
        (zigurat.core/nns \"schools\"))"
  (comp read-tree read-string parse-string))

(def make-trees (partial map str->tree))

(def parse-tree (comp make-trees treebank-parser))
