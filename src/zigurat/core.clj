(ns zigurat.core
  "A translator from natural language forms to the cypher query language.

  * A sentence is a list of phrases
  * A phrase is a hashmap with `:phrase` and `:tag` members"

  (:require [zigurat.nlp                   :refer [parse-tree]]
            [zigurat.protocols.grammar     :refer :all]
            [zigurat.protocols.react-graph :refer :all]
            [zigurat.types.sem-graph       :refer :all]
            [clojure.pprint                :refer [pprint]]))

(def map-eval (partial map eval))
(def realize-tree (comp map-eval parse-tree))





(realize-tree ["rural schools in Santiago of Chile"]
                )






(comment

  (realize-tree ["rural schools in Santiago of Chile"]
                )

  (pprint (realize-tree ["rural schools in Santiago of Chile"]
                        ))


  (pprint (parse-tree ["rural schools located in coastal cities of Chile and with more than fifty students"]
                      ))

  (realize-tree ["rural schools located in coastal cities of Chile and with more than fifty students"])


  (->> ["rural schools in Santiago of Chile"]
       parse-tree
       pprint)

  (parse-tree ["rural schools in Santiago of Chile"]
              )

  (map eval (parse-tree ["rural schools in Santiago of Chile."]))

  (time
   (let [x (parse-tree ["rural schools in Santiago of Chile"])]
     (dotimes [_ 1000000]
       (map eval x))))

  (time
   (let [x ["rural schools in Santiago of Chile"]]
     (dotimes [_ 10]
       (map eval (parse-tree x)))))

  (time
   (let [x ["rural schools in Santiago of Chile"]]
     (dotimes [_ 100]
       (parse-tree x))))

  (time
   (let [x ["rural schools in Santiago of Chile"]]
     (dotimes [_ 100]
       (np (np (jj "rural")
               (nns "schools"))
           (pp (in "in")
               (np (np (nnp "Santiago"))
                   (pp (in "of")
                       (np (nnp "Chile")))))))))


  ;; (extend-type String
  ;;   Grammar
  ;;   (jj  [word] (->SemanticGraphNode (gensym "n") #{word} {} #{} #{}))
  ;;   (nns [word] (->SemanticGraphNode (gensym "n") #{word} {} #{} #{}))
  ;;   (in  [word] (->SemanticGraphEdge (gensym "e") #{:in} {} #{} #{}))
  ;;   (nnp [word] (->SemanticGraphNode (gensym "n") #{} {:name word} #{} #{}))
  ;;   (cc  [word] (->SemanticGraphCoor :and))
  ;;   )


  )
