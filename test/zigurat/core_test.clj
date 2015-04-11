(ns zigurat.core-test
  (:require [clojure.test :refer [run-tests]]
            [zigurat.nlp  :refer [parse-tree]]
            [zigurat.core :refer [realize-tree
                                  top np  pp qp
                                  jj  nns nnp in rbr cd
                                  comma]]
            [clojure.pprint :refer [pprint]]))

(run-tests 'zigurat.core)

;;
;; Sandbox
;;

(pprint (parse-tree ["rural schools in Santiago of Chile , with more than five students"]))
(pprint (parse-tree ["a book has as first author a person"]))
(pprint (parse-tree ["let x be a book where : x has as first author a person"]))
(pprint (parse-tree ["let x be a book" "x has as first author a person"]))

;;(pprint (realize-tree ["rural schools in Santiago of Chile , with more than five students"]))


;;(time
;;   (let [x ["rural schools in Santiago of Chile"]]
;;     (dotimes [_ 100]
;;       (realize-tree x))))

 (time
    (let [x (parse-tree ["rural schools in Santiago of Chile"])]
      (dotimes [_ 1000000]
        (map eval x))))
