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


(defn ->type-paths [graph]
  (map (fn [path] (map #(a/attr graph % :type) path)) (distinct-paths graph)))


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
    ))
