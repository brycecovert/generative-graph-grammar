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
       (add-typed-node :start 1))
   (-> (l/digraph )
       (add-typed-node :replaced 1))])

(def start->end==>start->added->end
  [(-> (l/digraph )
       (add-typed-edge :start 1 :end 2))
   (-> (l/digraph )
       (add-typed-edge :start 1 :added 3)
       (add-typed-edge :added 3 :end 2))])

(def middle->end==>middle-1->middle-2->end
  [(-> (l/digraph )
       (add-typed-edge :middle 1 :end 3))
   (-> (l/digraph )
       (add-typed-edge :middle-1 1 :middle-2 2 )
       (add-typed-edge :middle-2 2 :end 3 ))])

(def start->middle==>start->middle-1->middle-2
  [(-> (l/digraph )
       (add-typed-edge :start 1 :middle 3)
       )
   (-> (l/digraph )
       (add-typed-edge :start 1 :middle-1 2)
       (add-typed-edge :middle-1 2 :middle-2 3))])

(def ?->end==>?->middle->end
  [(-> (l/digraph )
       (add-typed-edge nil 1 :end 3))
   (-> (l/digraph )
       (add-typed-edge nil 1 :middle 2)
       (add-typed-edge :middle 2 :end 3))])


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
                         (add-typed-edge :start :start :fork :fork))]
        (is (= {:start :start :fork :fork} 
               (search-for-subgraph graph searcher))))
      (let [searcher (-> (l/digraph)
                         (add-typed-edge :fork :fork :end :end))]
        (is (= {:end :end :fork :fork} 
               (search-for-subgraph graph searcher))))
      (let [searcher (-> (l/digraph)
                         (add-typed-edge :start :start :fork :fork)
                         (add-typed-edge :fork :fork :end :end))]
        (is (= {:start :start :fork :fork :end :end}  
               (search-for-subgraph graph searcher)))))

    (testing "node match"
      (let [searcher (-> (l/digraph)
                         (add-typed-node :start :start))]
        (is (= {:start :start} 
               (search-for-subgraph graph searcher))))
      (let [searcher (-> (l/digraph)
                         (add-typed-node :non :non))]
        (is (nil? (search-for-subgraph graph searcher))))

      (testing "duplicate nodes of type"
        (let [searcher (-> (l/digraph)
                           (add-typed-edge :climax :climax-1 :climax :climax-2))]
          (is (= {:climax-1 :climax
                  :climax-2 :climax-2} 
                 (search-for-subgraph graph searcher))))))

    (testing "partial match"
      (let [searcher (-> (l/digraph)
                         (add-typed-edge :start :start :shouldnt-exist :shouldnt-exist))]
        (is (= nil
               (search-for-subgraph graph searcher))))
      (let [searcher (-> (l/digraph)
                         (add-typed-edge :start :start :climax :climax))]
        (is (= nil
               (search-for-subgraph graph searcher)))))
    (testing "should match on arbirtrary wildcards"
      (let [searcher (-> (l/digraph)
                         (add-typed-edge nil :middle :climax :climax-1 )
                         (add-typed-edge :climax :climax-1 :climax :climax-2  )
                         (add-typed-edge :climax :climax-2 nil :end  ))]
        (is (= {:middle :middle
                :climax-1 :climax
                :climax-2 :climax-2
                :end :end}
               (search-for-subgraph graph searcher)))))))


(deftest a-test
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
               ->type-paths))))
    (testing "replacing middle should preserve edge"
      (is (=
           [[:start :replaced :end]]
           (-> (l/digraph)
               (add-typed-node :start 1)
               (add-typed-node :middle 2)
               (add-typed-node :end 3)
               (l/add-edges [1 2] [2 3])
               (apply-rule [(-> (l/digraph)
                                (add-typed-edge :middle 2 :end 3))
                            (-> (l/digraph)
                                (add-typed-edge :replaced 2 :end 3))])
               
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

    (testing "shouldn't insert forks"
      (is (=
           [[:start :middle :middle :end]]
           (-> (l/digraph)
               (add-typed-edge :start :start :middle :middle)
               (add-typed-edge :middle :middle :end :end)
               (apply-rule [(-> (l/digraph)
                                (add-typed-edge :start :start :middle :middle-2))
                            (-> (l/digraph)
                                (add-typed-edge :start :start :middle :middle-1)
                                (add-typed-edge :middle :middle-1 :middle :middle-2))])
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
               ->type-paths))))))
