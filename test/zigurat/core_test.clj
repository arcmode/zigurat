(ns zigurat.core-test
  (:require [clojure.test :refer [run-tests]]
            [zigurat.nlp  :refer [parse-tree]]
            [zigurat.core :refer [realize-tree]]))

(run-tests 'zigurat.core)


;;
;; Sandbox
;;

(parse-tree ["rural schools in Santiago of Chile"])

(realize-tree ["rural schools in Santiago of Chile"])

(time
   (let [x ["rural schools in Santiago of Chile"]]
     (dotimes [_ 100]
       (realize-tree x))))

(time
   (let [x (parse-tree ["rural schools in Santiago of Chile"])]
     (dotimes [_ 1000000]
       (map eval x))))
