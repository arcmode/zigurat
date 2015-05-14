(ns zigurat.core-test
  (:require [clojure.test :refer [deftest is]]
            [zigurat.core :refer [parse-tree realize-tree]]))

(defn test-statement
  [[sym docstr]]
  (let [slice-examples (fn [docstr] (drop 1 (clojure.string/split docstr #"\n\s*=>")))
        examples (slice-examples docstr)
        wrap-in-parens (fn [exa-str] (str "(" exa-str ")"))
        parse-example (comp read-string wrap-in-parens)
        exa-code-list (map parse-example examples)
        read-is-statement (fn [exa-code-pair] `(is (= ~(first exa-code-pair) ~(second exa-code-pair))))
        is-statements (map read-is-statement exa-code-list)
        test-name (symbol (str (name sym) "-test-"))
        test-code `(deftest ~test-name ~@is-statements)
        ]
    test-code))

(defn ns-publics-docstr
  [nspace]
  (for [[name var] (ns-publics nspace)]
    [name (:doc (meta var))]))

(defmacro doctest-fn
  [fn-sym]
  (test-statement [fn-sym (:doc (eval `(meta (var ~fn-sym))))]))

(defmacro doctest-ns
  [ns-sym]
  (map test-statement (filter second (ns-publics-docstr ns-sym))))



(doctest-ns zigurat.core)


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
