(ns graph-grammar.core-test
  (:require [clojure.test :refer :all]
            [graph-grammar.core :refer :all]
            [loom.graph :as l]
            [loom.io :as lio]
            [loom.alg :as alg]
            [loom.attr :as a]
            [loom.derived :as d]))

(def start==>replaced
  [(-> (l/digraph )
       (l/add-nodes [:start 1]))
   (-> (l/digraph )
       (l/add-nodes [:replaced 1]))])

(def start->end==>start->added->end
  [(-> (l/digraph )
       (l/add-edges [[:start 1] [:end 2]]))
   (-> (l/digraph )
       (l/add-edges [[:start 1] [:added 3]]
                    [[:added 3] [:end 2]]))])

(def middle->end==>middle-1->middle-2->end
  [(-> (l/digraph )
       (l/add-edges [[:middle 1] [:end 3]]))
   (-> (l/digraph )
       (l/add-edges [[:middle-1 1] [:middle-2 2]]
                    [[:middle-2 2] [:end 3]]))])

(def start->middle==>start->middle-1->middle-2
  [(-> (l/digraph )
       (l/add-edges [[:start 1] [:middle 3]]))
   (-> (l/digraph )
       (l/add-edges [[:start 1] [:middle-1 2]]
                    [[:middle-1 2] [:middle-2 3]]))])

(def ?->end==>?->middle->end
  [(-> (l/digraph )
       (l/add-edges [[nil 1] [:end 3]]))
   (-> (l/digraph )
       (l/add-edges [[nil 1] [:middle 2]]
                    [[:middle 2] [:end 3]]))])


(defn ->type-paths [graph]
  (map (fn [path] (map #(a/attr graph % :type) path)) (distinct-paths graph)))

(deftest find-match
  (let [graph (-> (l/digraph)
                    (add-typed-node :start :start)
                    (add-typed-node :middle :middle)
                    (add-typed-node :climax :climax)
                    (add-typed-node :climax :climax-2)
                    (add-typed-node :end :end)
                    (add-typed-node :fork :fork)
                    (l/add-edges [:start :middle]
                                 [:middle :climax]
                                 [:climax :climax-2]
                                 [:climax-2 :end]
                                 [:start :fork]
                                 [:fork :end]))]
    
    (testing "simple match"
      (let [searcher (-> (l/digraph)
                         (l/add-edges [[:start :start] [:fork :fork]]))]
        (is (= {:start :start :fork :fork} 
               (search-for-subgraph graph searcher))))
      (let [searcher (-> (l/digraph)
                         (l/add-edges [[:fork :fork] [:end :end]]))]
        (is (= {:end :end :fork :fork} 
               (search-for-subgraph graph searcher))))
      (let [searcher (-> (l/digraph)
                         (l/add-edges [[:start :start] [:fork :fork]]
                                      [[:fork :fork] [:end :end]]))]
        (is (= {:start :start :fork :fork :end :end}  
               (search-for-subgraph graph searcher)))))

    (testing "node match"
      (let [searcher (-> (l/digraph)
                         (l/add-nodes [:start :start]))]
        (is (= {:start :start} 
               (search-for-subgraph graph searcher))))
      (let [searcher (-> (l/digraph)
                         (l/add-nodes [:non :non]))]
        (is (nil? (search-for-subgraph graph searcher))))

      (testing "duplicate nodes of type"
        (let [searcher (-> (l/digraph)
                           (l/add-edges [[:climax :climax-1] [:climax :climax-2]]))]
          (is (= {:climax-1 :climax
                  :climax-2 :climax-2} 
                 (search-for-subgraph graph searcher))))))

    (testing "partial match"
      (let [searcher (-> (l/digraph)
                         (l/add-edges [[:start :start] [:shouldnt-exist :shouldnt-exist]]))]
        (is (= nil
               (search-for-subgraph graph searcher))))
      (let [searcher (-> (l/digraph)
                         (l/add-edges [[:start :start] [:climax :climax]]))]
        (is (= nil
               (search-for-subgraph graph searcher)))))
    (testing "should match on arbirtrary wildcards"
      (let [searcher (-> (l/digraph)
                         (l/add-edges [[nil :middle] [:climax :climax-1] ]
                                      [[:climax :climax-1] [:climax :climax-2] ]
                                      [[:climax :climax-2] [nil :end]]))]
        (is (= {:middle :middle
                :climax-1 :climax
                :climax-2 :climax-2
                :end :end}
               (search-for-subgraph graph searcher)))))))


(deftest a-test
  (testing "matching subsequences"
    (is (subsequence-matches? [1 2 3]
                              [1 2 3]))
    (testing "should consider nil a wildcard"
      (is (subsequence-matches? [nil 2 3]
                                [1 2 3]))))

  (testing "finding sections that apply"
    )
  (testing "applying rules"
    (testing "root node"
      (is (=
           1
           (-> (l/digraph)
               (add-typed-node :start 1)
               (root)
               ))))
    (testing "distinct paths"
      (is (=
           [[1]]
           (-> (l/digraph)
               (add-typed-node :start 1)
               (distinct-paths)
               ))))
    (testing "single replacement"
      (is (=
           [[:replaced]]
           (-> (l/digraph)
               (add-typed-node :start 1)
               (apply-rule start==>replaced)
               ->type-paths)

           )))

    (testing "edge replacement"
      (is (=
           [[:start :added :end]]
           (-> (l/digraph)
               (add-typed-node :start 1)
               (add-typed-node :end 2)
               (l/add-edges [1 2])
               (apply-rule start->end==>start->added->end)
               ->type-paths)
           )))
    (testing "replacing middle should preserve edge"
      (is (=
           [[:start :replaced :end]]
           (-> (l/digraph)
               (add-typed-node :start 1)
               (add-typed-node :middle 2)
               (add-typed-node :end 3)
               (l/add-edges [1 2] [2 3])
               (apply-rule [(-> (l/digraph)
                                (l/add-edges [[:middle 2] [:end 3]]))
                            (-> (l/digraph)
                                (l/add-edges [[:replaced 2] [:end 3]]))])
               
               ->type-paths)
           )))

    (testing "original incoming edges preserved"
      (is (=
           [[:start :middle-1 :middle-2 :end]]
           (-> (l/digraph)
               (add-typed-node :start 1)
               (add-typed-node :middle 2)
               (add-typed-node :end 3)
               (l/add-edges [1 2] [2 3])
               (apply-rule middle->end==>middle-1->middle-2->end)
               ->type-paths)
           )))

    (testing "original outgoing edges preserved"
      (is (=
           [[:start :middle-1 :middle-2 :end]]
           (-> (l/digraph)
               (add-typed-node :start 1)
               (add-typed-node :middle 2)
               (add-typed-node :end 3)
               (l/add-edges [1 2] [2 3])
               (apply-rule start->middle==>start->middle-1->middle-2)
               ->type-paths)
           )))

    (testing "should allow wildcard "
      (println "this test")
      (is (=
           [[:start :middle :end]]
           (-> (l/digraph)
               (add-typed-node :start 1)
               (add-typed-node :end 2)
               (l/add-edges [1 2])
               (apply-rule ?->end==>?->middle->end)
               ->type-paths)
           )))

    (testing "should allow something"
      (let [move-lock-towards-entrance [(-> (l/digraph)
                                            (l/add-edges [[nil 1] [:lock 4]]
                                                         [[nil 2] [nil 3]]
                                                         #_[[nil 3] [:lock 4]]))
                                        (-> (l/digraph)
                                            (l/add-edges [[nil 1] [:lock 4]]
                                                         [[nil 2] [nil 3]]
                                                         #_[[nil 2] [:lock 4]]))]]
        #_(is (=
             [[:start :key :lock :end]]
             (-> (l/digraph)
                 (add-typed-node :start 0)
                 (add-typed-node :task 1)
                 (add-typed-node :lock 2)
                 (add-typed-node :task 3)
                 (add-typed-node :task 4)
                 (l/add-edges [0 1] [1 2] [3 4] [4 2] [0 3])
                 (doto (#(view %)))
                 (apply-rule move-lock-towards-entrance)
                 (doto (#(view %)))
                 ->type-paths
                 )))))
    ))
