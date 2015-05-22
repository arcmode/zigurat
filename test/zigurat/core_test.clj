(ns zigurat.core-test
  (:require [doctest.core :refer [doctest-ns]]
            [zigurat.core :refer [parse-tree
                                  realize-tree
                                  if-apply
                                  read-tree
                                  read-node
                                  parse-string
                                  str->tree]]))

(doctest-ns zigurat.core)
(clojure.pprint/pprint (macroexpand-1 '(doctest-ns zigurat.core)))

;;
;; Sandbox
;;

(clojure.pprint/pprint (parse-tree ["rural schools in Santiago of Chile , with more than five students"]))
(clojure.pprint/pprint (parse-tree ["the first author of a book is a person"]))
(clojure.pprint/pprint (parse-tree ["let x be a book whose first author is a person named Author"]))

(clojure.pprint/pprint (realize-tree ["rural schools in Santiago of Chile , with more than five students"]))
(clojure.pprint/pprint (parse-tree ["rural schools located in Santiago of Chile , with more than five students"]))
(clojure.pprint/pprint (parse-tree ["friends from Santiago of Chile , with more than five friends"]))

;;(clojure.pprint/pprint (realize-tree ["let x be a book whose first author is a person named Author ."]))

;;(time
;;   (let [x ["rural schools in Santiago of Chile"]]
;;     (dotimes [_ 100]
;;       (realize-tree x))))

(time
 (let [x (parse-tree ["rural schools in Santiago of Chile , with less than five students"])]
   (dotimes [_ 1000000]
     (map eval x))))
