(ns zigurat.protocols.react-graph)

;; [todo] split into graph, edge and node protocols
(defprotocol ReactiveGraph
  "A protocol for reactive graphs."
  (get-node            [elem])
  (get-edge            [elem])
  (bind-node-to-source [elem node])
  (bind-incoming-edge  [elem edge]))
