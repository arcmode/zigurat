(ns zigurat.core-test
  (:require [clojure.test :refer [run-tests]]
            [zigurat.nlp  :refer [parse-tree]]
            [zigurat.core :refer [realize-tree
                                  top np  pp
                                  jj  nns nnp in
                                  comma]]))

(run-tests 'zigurat.core)

;;
;; Sandbox
;;

(clojure.pprint/pprint (parse-tree ["rural schools in Santiago of Chile , with more than five students"]))
(clojure.pprint/pprint (parse-tree ["a book has as first author a person"]))
(clojure.pprint/pprint (parse-tree ["let x be a book where : x has as first author a person"]))
(clojure.pprint/pprint (parse-tree ["let x be a book" "x has as first author a person"]))

(clojure.pprint/pprint (realize-tree ["rural schools in Santiago"]))


;;(time
;;   (let [x ["rural schools in Santiago of Chile"]]
;;     (dotimes [_ 100]
;;       (realize-tree x))))

 (time
    (let [x (parse-tree ["rural schools in Santiago of Chile"])]
      (dotimes [_ 1000000]
        (map eval x))))
