(ns zigurat.protocols.grammar)

;; [todo] split into categories
(defprotocol Grammar
  "A protocol for grammars."
  (top [p])
  (s   [np vp])
  (vp  [p]
       [p1 p2]
       [p1 p2 p3])
  (np  [p]
       [p1 p2])
  (pp  [p1 p2])
  (qp  [jjr in cd])

  (in->pp [np in])
  (jj->np [nns jj]))


(defprotocol Token
  (get-token [x]))
