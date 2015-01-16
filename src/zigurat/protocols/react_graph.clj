(ns zigurat.protocols.react-graph)

;; [todo] split into graph, edge and node protocols
(defprotocol ReactiveGraph
  "A protocol for reactive graphs."
  (bind-with-node [elem node])
  (bind-with-edge [elem edge])
  (bind-graph-to-node [elem graph node])
  (bind-graph-to-edge [elem graph edge])
  (add-label-to-link [elem label]))
