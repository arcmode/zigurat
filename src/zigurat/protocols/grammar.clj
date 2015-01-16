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

  (jj  [w])
  (nns [w])
  (in  [w])
  (nnp [w])
  (cc  [w])
  (vbn [w])
  (jjr [w]))
