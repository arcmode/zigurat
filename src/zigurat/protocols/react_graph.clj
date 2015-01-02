(ns zigurat.protocols.react-graph)

;; [todo] split into graph, edge and node protocols
(defprotocol ReactiveGraph
  "A protocol for reactive graphs."
  (bond-with-node [elem node])
  (bond-with-edge [elem edge])
  (bond-graph-to-node [elem graph node])
  (bond-graph-to-edge [elem graph edge]))
