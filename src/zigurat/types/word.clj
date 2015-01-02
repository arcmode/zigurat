(ns zigurat.types.word
  (:require [zigurat.protocols.grammar    :refer [Grammar]]
            [zigurat.types.sem-graph :refer [->SemanticGraphNode
                                                  ->SemanticGraphEdge]]))

(deftype Word [token]
  Grammar
  (jj  [word] (->SemanticGraphNode (gensym "n") #{token} {} #{} #{}))
  (nns [word] (->SemanticGraphNode (gensym "n") #{token} {} #{} #{}))
  (in  [word] (->SemanticGraphEdge (gensym "e") #{:in} {} #{} #{}))
  (nnp [word] (->SemanticGraphNode (gensym "n") #{} {:name token} #{} #{}))
  )
