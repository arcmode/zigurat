(ns zigurat.protocols.grammar)

;; [todo] split into categories
(defprotocol Grammar
  "A protocol for grammars."
  (top [phrase])
  (np  [phrase]
       [phrase-a phrase-b])
  (pp  [phrase-a phrase-b])

  (jj  [word])
  (nns [word])
  (in  [word])
  (nnp [word]))
