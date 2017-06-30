(ns graph-grammar.core
  (:require [loom.graph :as l]
            [loom.io :as lio]
            [loom.alg :as alg]
            [loom.attr :as a]
            [loom.derived :as d]))
(defn dbg [n]
  (println n)
  n)

(defn id-node [n]
  (assoc n :id (str (java.util.UUID/randomUUID))))

(def start-node (id-node {:type :start}))

(defn add-typed-node
  ([g type]
   (add-typed-node g type (str (java.util.UUID/randomUUID))))
  ([g type id]
   (-> g
       (l/add-nodes id)
       (a/add-attr-to-nodes :type type [id]))))

(def initial (-> (l/digraph)
                 (add-typed-node :start )))


(def non-terminal-nodes
  #{#_:gate :chain-linear :chain-parallel :chain :hook :fork :start :chain-final :get-spell-component :initial-task})

(def single-puzzle-rules
  {:initial [(-> (l/digraph)
                 (l/add-nodes [:start 1]))
             (-> (l/digraph)
                 (l/add-edges [[:begin 1] [:task 2]] 
                              [[:task 2] [:task 3]]
                              [[:task 3] [:task 4]]
                              [[:task 4] [:task 5]]
                              [[:task 5] [:task 6]]
                              [[:task 6] [:task 7]]
                              [[:task 7] [:task 8]]
                              [[:task 8] [:end 9]]))]
   :add-task [(-> (l/digraph)
                  (l/add-edges [[:task 1] [:task 2]]))
              (-> (l/digraph)
                  (l/add-edges [[:task 1] [:task 2]]
                               [[:task 2] [:task 3]]))]

   ;; rule 1
   :generate-lock [(-> (l/digraph)
                       (l/add-edges [[nil 1] [:task 2]]))
                   (-> (l/digraph)
                       (l/add-edges [[nil 1] [:key 3]]
                                    [[nil 1] [:lock 2]]
                                    [[:key 3] [:lock 2]]))]


   ;; rule 2
   :move-lock [(-> (l/digraph)
                   (l/add-edges [[nil 1] [:lock 4]]
                                [[nil 2] [nil 3]]
                                [[nil 3] [:lock 4]]))
               (-> (l/digraph)
                   (l/add-edges [[nil 1] [:lock 4]]
                                [[nil 2] [nil 3]]
                                [[nil 2] [:lock 4]]
                                [[nil 3] [:lock 4]]))]


   ;; rule 4
   :move-key [(-> (l/digraph)
                  (l/add-edges [[nil 1] [:key 2]]
                               [[:key 2] [:lock 3]]
                               [[:lock 3] [:task 4]]
                               [[:task 4] [nil 5]]))
              (-> (l/digraph)
                  (l/add-edges [[nil 1] [:task 4]]
                               [[:task 4] [:key 2]]
                               [[:key 2] [:lock 3]]
                               [[:lock 3] [nil 5]]))]

   ;; rule 5
   :duplicate-key [(-> (l/digraph)
                       
                       (l/add-edges [[:task :task] [:lock :lock]]
                                    [[:key :key] [:lock :lock]]))
                   (-> (l/digraph)
                       (l/add-edges [[:task :task] [:key :new-key]]
                                    [[:task :task] [:lock :lock]]
                                    [[:key :key] [:lock :lock]]
                                    [[:key :new-key] [:lock :lock]]))]

   ;; rule 6
   :duplicate-lock [(-> (l/digraph)
                        
                        (l/add-edges [[:key :key] [:lock :lock]]
                                     [[:lock :lock] [nil 1]]
                                     [[nil 1] [:task :task]]))
                    (-> (l/digraph)
                        (l/add-edges [[:key :key] [:lock :lock]]
                                     [[:lock :lock] [nil 1]]
                                     [[nil 1] [:lock :task]]
                                     [[:key :key] [:lock :task]]))]})

(def recipe [[[:initial] 1 1]
             [[:add-task] 1 5]
             [[:generate-lock :move-lock :move-key :duplicate-key :duplicate-lock] 10 15]])




(defn root [graph]
  (first
   (filter
    (fn [n] (not (seq (l/predecessors graph n))))
    (l/nodes graph))))

(defn end [graph]
  (first
   (filter
    (fn [n] (not (seq (l/successors graph n))))
    (l/nodes graph))))

(defn distinct-paths
  ([graph]
   (distinct-paths graph (root graph)))
  ([graph start-node]
   (distinct-paths graph start-node [start-node] []))
  ([graph node path paths]
   (let [successors (l/successors graph node)]

     (if (seq successors)
       (into paths
             (mapcat (fn [n]
                       (distinct-paths graph n (conj path n) []))
                     (l/successors graph node)))
       (conj  paths path)))))


(defn search [graph subgraph [success? assignments]]
  (if (>= (count assignments) (count (l/nodes subgraph)))
    [(and (every?
           (set (l/edges graph))
           (map (fn [[f t]]
                  [(assignments (second f)) (assignments (second t))])
                (l/edges subgraph))))
     assignments]
    (let [is-unassigned? (complement (set (keys assignments)))
          [next-assignment-type next-assignment] (->> (l/nodes subgraph)
                                                      (filter (fn [n] (is-unassigned? (second n))))
                                                      first)]
      
      (reduce
       (fn [[success? assignments] possible-assignment]
         
         (if (and (not ((set (vals assignments)) possible-assignment))
                  (or (= next-assignment-type
                         (a/attr graph possible-assignment :type))
                      (nil? next-assignment-type)))
           (let [[success? test-assignments] (search graph subgraph [false (assoc assignments next-assignment possible-assignment)])]
             (if success?
               (reduced [success? test-assignments])
               [false assignments]))
           
           [false assignments]))
       [success?  assignments]
       (shuffle (l/nodes graph))))))

(defn search-for-subgraph
  ([graph subgraph]
   (let [[success? assignments] (search graph subgraph [false {}])]
     (if success?
       assignments
       nil))))



(defn add-nodes-from-output-graph [graph output-graph target-ids]
  (reduce
   (fn [graph [type id]]
     (if type
       
       (add-typed-node graph  type (target-ids id))
       graph))
   graph
   (l/nodes output-graph)))

(defn add-edges-from-output-graph [graph output-graph target-ids]
  (reduce
   (fn [graph [[_ from-id] [_ to-id]]]
     (l/add-edges graph [(target-ids from-id) (target-ids to-id)]))
   graph
   (l/edges output-graph)))

(defn add-edges-from-old-nodes [graph output-graph target-ids original-graph]
  (let [source-ids (into {} (map (fn [[from to]] [to from]) target-ids))
        original-edges (->> (l/nodes original-graph)
                            (filter (set (vals target-ids)))
                            (filter (set (l/nodes output-graph))) ;; IS THIS RIGHT?
                            ;; Seems like always false
                            (mapcat #(map (fn [s] [(source-ids %) s]) (l/successors original-graph %)))
                            #_(filter (complement (comp (set (l/nodes output-graph)) second))))]
    (apply l/add-edges graph original-edges)
    ))

(defn remove-edges-from-input [graph input target-ids]

  (->> (l/edges input)
       (map (fn [[[_ from] [_ to]]] [(target-ids from) (target-ids to)]))
       (apply l/remove-edges graph)))

(defn apply-rule [graph [input output]]
  (if-let [matched-nodes (search-for-subgraph graph input)]
    (let [original-graph graph
          target-ids (into matched-nodes
                           (map (fn [[type n]]
                                  [n  (matched-nodes n (str (java.util.UUID/randomUUID)))]))
                           (l/nodes output))]
      (-> graph
          (add-nodes-from-output-graph output target-ids)

          (remove-edges-from-input input target-ids)
          (add-edges-from-output-graph output target-ids)
          (add-edges-from-old-nodes output target-ids original-graph)))
    graph))

(defn view [graph]
  (let [ids (into {} (map vector  (alg/bf-traverse graph (root graph)) (range)))]
    (lio/view
     graph
     :node-label #(str (name (or  (a/attr graph % :type) :unknown)) " " (ids %)))))

(defn make-graph [recipe rules]
  (reduce
   (fn [graph [rule-keys min max]]
     (let [random-iterations (+ min (rand-int (- max min)))]
       (loop [x 0
              graph graph]
         (if (< x random-iterations)
           (recur (inc x)
                  (apply-rule graph ((rand-nth rule-keys) rules)))
           (apply-rule graph ((rand-nth rule-keys) rules)))))) 
   initial 
   recipe))
