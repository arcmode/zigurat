(ns zigurat.core-test
  (:require [zigurat.core :refer :all]
            [clojure.test :refer :all]))

(deftest test-tag-sentences
  (testing "it returns a list containing one list of tagged phrases per input sentence"
    (let [one-sentence             (tag-sentences "Rural schools in Santiago of Chile.")
          another-sentence         (tag-sentences "Architects in rural projects.")
          two-more-sentences       (tag-sentences "Rural schools in Santiago of Chile. Architects in rural projects.")
          count-one-sentence       (count one-sentence)
          count-another-sentence   (count another-sentence)
          count-two-more-sentences (count two-more-sentences)]
      (is (= '(({:phrase ["Rural" "schools"],  :tag "NP"}
                {:phrase ["in"],               :tag "PP"}
                {:phrase ["Santiago"],         :tag "NP"}
                {:phrase ["of"],               :tag "PP"}
                {:phrase ["Chile"],            :tag "NP"}))
             one-sentence))
      (is (= '(({:phrase ["Architects"],       :tag "NP"}
                {:phrase ["in"],               :tag "PP"}
                {:phrase ["rural" "projects"], :tag "NP"}))
             another-sentence))
      (is (= '(({:phrase ["Rural" "schools"],  :tag "NP"}
                {:phrase ["in"],               :tag "PP"}
                {:phrase ["Santiago"],         :tag "NP"}
                {:phrase ["of"],               :tag "PP"}
                {:phrase ["Chile"],            :tag "NP"})
               ({:phrase ["Architects"],       :tag "NP"}
                {:phrase ["in"],               :tag "PP"}
                {:phrase ["rural" "projects"], :tag "NP"}))
             two-more-sentences))
      (is (= 1 count-one-sentence))
      (is (= 1 count-another-sentence))
      (is (= 2 count-two-more-sentences)))))

(deftest test-cypher-sentences
  (testing "it returns a list containing one cypher query per input sentence"
    (with-redefs [zigurat.core/tag-sentences (constantly '(({:phrase ["Rural" "schools"],  :tag "NP"}
                                                            {:phrase ["in"],               :tag "PP"}
                                                            {:phrase ["Santiago"],         :tag "NP"}
                                                            {:phrase ["of"],               :tag "PP"}
                                                            {:phrase ["Chile"],            :tag "NP"})))]
      (is (= '("MATCH (_n1:Rural:School)
                      -[:IN]->
                      (_n2:Location {name: 'Santiago'})
                      -[:IN]->
                      (_n3:Location {name: 'Chile'})
                RETURN _n1, _n2, _n3")
             (cypher-sentences "Rural schools in Santiago of Chile"))))))


;;               "MATCH (_a1:Architect)
;;                      -[:IN]->
;;                      (_l1:Location {name: 'Santiago'})
;;                      -[:IN]->
;;                      (_l2:Location {name: 'Chile'})
;;                RETURN _rs1, _l1, _l2"))
