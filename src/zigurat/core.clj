(ns zigurat.core
  "A translator from natural language forms to the cypher query language.

  * A sentence is a list of phrases
  * A phrase is a hashmap with `:phrase` and `:tag` members"

  (:require [zigurat.nlp                   :refer [parse-tree]]

            [zigurat.protocols.grammar     :refer :all]
            [zigurat.protocols.react-graph :refer :all]
            [zigurat.types.sem-graph       :refer :all]
            [zigurat.types.word            :refer :all]))


(def realize-tree (comp (partial map eval) parse-tree))

(-> ["rural schools in Santiago of Chile ."]
    parse-tree
    )

(realize-tree ["rural schools in Santiago of Chile"])


(map eval (parse-tree ["rural schools in Santiago of Chile"]))

(time
 (let [x (parse-tree ["rural schools in Santiago of Chile"])]
   (dotimes [_ 1000000]
     (map eval x))))



(comment


(np (->SemanticGraphNode (gensym "n") #{:location} {:name "Chile"}  #{} #{})
    (->SemanticGraphNode (gensym "n") #{:country} {:alias "Chilito"} #{} #{}))

(in "of")

(np (nnp "Chile"))

(np (nnp "Santiago"))

(np (nnp "Santiago"))

(pp (in "of") (np (nnp "Chile")))

(np (np (nnp "Santiago"))
    (pp (in "of")
        (np (nnp "Chile"))))

(pp (in "in")
    (np (np (nnp "Santiago"))
        (pp (in "of")
            (np (nnp "Chile")))))

(np (jj "rural")
  (nns "schools"))

(np (np (jj "rural")
        (nns "schools"))
    (pp (in "in")
        (np (np (nnp "Santiago"))
            (pp (in "of")
                (np (nnp "Chile"))))))


(top (np (np (jj "rural")
             (nns "schools"))
         (pp (in "in")
             (np (np (nnp "Santiago"))
                 (pp (in "of")
                     (np (nnp "Chile")))))))



(realize-tree ["rural schools in Santiago of Chile"])

(class (realize-tree ["rural schools in Santiago of Chile"]))

(time
 (let [x ["rural schools in Santiago of Chile"]]
   (dotimes [_ 100]
     (realize-tree x))))

(time
 (let [x (parse-tree ["rural schools in Santiago of Chile"])]
   (dotimes [_ 100]
     (map eval x))))

(time
 (let [x ["rural schools in Santiago of Chile"]]
   (dotimes [_ 100]
     (np (np (jj "rural")
             (nns "schools"))
         (pp (in "in")
             (np (np (nnp "Santiago"))
                 (pp (in "of")
                     (np (nnp "Chile")))))))))

;; (time
;;  (let [x ["Malloco is a rural location in Central Chile" "rural schools in Santiago of Chile"]]
;;    (dotimes [_ 100]
;;      (eval-tree '(np (np (jj "rural")
;;                     (nns "schools"))
;;                 (pp (in "in")
;;                     (np (np (nnp "Santiago"))
;;                         (pp (in "of")
;;                             (np (nnp "Chile"))))))))))


;; (time
;;  (let [x ["Malloco is a rural location in Central Chile" "rural schools in Santiago of Chile"]]
;;    (dotimes [_ 100]
;;      (eval (parse-tree x)))))

(-> ["rural schools in Santiago of Chile ."]
    parse-tree
    )

(-> ["rural schools in Santiago of Chile having more than five teachers with a PHD ."]
    parse-tree
    )

(-> ["A pizza with lots of pepperoni, pineapple, capsicum, mushrooms, anchovies, olives, and vegemite ."]
    parse-tree)

(comment
(top (s (s (s (np (prp "He"))
              (vp (vbd "ate")
                  (np (dt "a")
                      (nn "pizza"))
                  (pp (in "with")
                      (np (np (nns "lots"))
                          (pp (in "of")
                              (np (nn "pepperoni")
                                  (nn "pineapple")
                                  (nn "capsicum")))))))
           (vp (md "mushrooms")
               (vp (vb "anchovies")
                   (adjp (jj "olives")))))
        (cc "and")
        (s (np (nn "vegemite")))
        (. ".")))
)

;; (top (np (np (jj "rural")
;;              (nns "schools"))
;;          (pp (in "in")
;;              (np (np (nnp "Santiago"))
;;                  (pp (in "of")
;;                      (np (np (nnp "Chile"))
;;                          (pp (in "with")
;;                              (s (np (qp (jjr "more")
;;                                         (in "than")
;;                                         (cd "five"))
;;                                     (nns "teachers"))
;;                                 (vp (vbg "having")
;;                                     (np (dt "a")
;;                                         (nn "PHD")))))))))))


;; (top (np (np (jj "rural")
;;              (nns "schools")
;;          (pp (in "in")
;;              (np (np (nnp "Santiago"))
;;                  (pp (in "of")
;;                      (np (nnp "Chile"))))))))


;; (seq? {:a :b})


;; {:nodes {:n1 {:labels [:rural :schools]
;;               :out    [:e1]}
;;          :n2 {:attrs  {:name "Santiago"}
;;               :in     [:e1]
;;               :out    [:e2]}
;;          :n3 {:attrs  {:name "Chile"}}}
;;  :edges {:e1 {:labels [:in]
;;               :from   [:n1]
;;               :to     [:n2]}
;;          :e2 {:labels [:in]
;;               :from   [:n2]
;;               :to     [:n3]}}}


;; {:x1 {:type   :node
;;       :labels [:rural :schools]
;;       :out    [:x2]}
;;  :x2 {:type   :edge
;;       :labels [:in]
;;       :in     [:x1]
;;       :out    [:x3]}
;;  :x3 {:type   :node
;;       :attrs  {:name "Santiago"}
;;       :in     [:x2]
;;       :out    [:x4]}
;;  :x4 {:type   :edge
;;       :labels [:in]
;;       :in     [:x3]
;;       :out    [:x5]}
;;  :x5 {:type   :node
;;       :attrs  {:name "Chile"}
;;       :in     [:x4]}}


;; (->> ["rural"]
;;     parse-tree
;;      eval)


  )
