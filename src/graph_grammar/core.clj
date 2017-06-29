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
  [[(-> (l/digraph)
        (l/add-nodes [:start 1]))
    (-> (l/digraph)
        (l/add-edges [[:begin 1] [:task 2]]
                     [[:task 2] [:task 3]]
                     [[:task 3] [:task 4]]
                     [[:task 4] [:task 5]]
                     [[:task 5] [:task 6]]
                     [[:task 6] [:task 7]]
                     [[:task 7] [:task 8]]
                     [[:task 8] [:task 9]]
                     [[:task 9] [:task 10]]
                     [[:task 10] [:end 11]]
                     ))]

   ;; rule 1
   [(-> (l/digraph)
        (l/add-edges [[nil 1] [:task 2]]))
    (-> (l/digraph)
        (l/add-edges [[nil 1] [:key 3]]
                     [[nil 1] [:lock 2]]
                     [[:key 3] [:lock 2]]))]


   ;; rule 2
   [(-> (l/digraph)
        (l/add-edges [[nil 1] [:lock 4]]
                     [[nil 2] [nil 3]]
                     [[nil 3] [:lock 4]]))
    (-> (l/digraph)
        (l/add-edges [[nil 1] [:lock 4]]
                     [[nil 2] [nil 3]]
                     [[nil 2] [:lock 4]]))]


   ;; rule 4
   [(-> (l/digraph)
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
   [(-> (l/digraph)
       
        (l/add-edges [[:task :task] [:lock :lock]]
                     [[:key :key] [:lock :lock]]))
    (-> (l/digraph)
        (l/add-edges [[:task :task] [:key :new-key]]
                     [[:task :task] [:lock :lock]]
                     [[:key :key] [:lock :lock]]
                     [[:key :new-key] [:lock :lock]]))]

   ;; rule 6
   [(-> (l/digraph)
       
        (l/add-edges [[:key :key] [:lock :lock]]
                     [[:lock :lock] [nil 1]]
                     [[nil 1] [:task :task]]))
    (-> (l/digraph)
        (l/add-edges [[:key :key] [:lock :lock]]
                     [[:lock :lock] [nil 1]]
                     [[nil 1] [:lock :task]]
                     [[:key :key] [:lock :task]]))]
   ])


(def all-rules (-> [
                    ;; initial graph
                    [
                     (-> (l/digraph )
                         (l/add-nodes [:start 1]))
                     (-> (l/digraph [[:chain 1] [:gate 2]]
                                    [[:gate 2] [:chain 3]]
                                    [[:chain 3] [:gate 4]]
                                    [[:gate 4] [:chain 5]]
                                    [[:chain 5] [:gate 6]]
                                    [[:gate 6] [:end 7]]))]


                    ;; make linear chain
                    [(-> (l/digraph )
                         (l/add-edges [[:chain :start] [:gate :end]]))
                     (-> (l/digraph [[:chain-linear :start] [:chain-linear 2]]
                                    [[:chain-linear 2] [:chain-linear :end]]))]

                    [(-> (l/digraph )
                         (l/add-edges [[:chain :start] [:gate :end]]))
                     (-> (l/digraph [[:chain-linear :start] [:chain-linear 2]]
                                    [[:chain-linear 2] [:chain-linear 3]]
                                    [[:chain-linear 3] [:chain-linear :end]]))]

                    [(-> (l/digraph )
                         (l/add-edges [[:chain :start] [:gate 2]]))
                     (-> (l/digraph [[:chain-linear :start] [:chain-linear 2]]))]


                    [(-> (l/digraph )
                         (l/add-edges [[:chain :start] [:gate :end]]))
                     (-> (l/digraph )
                         (l/add-edges [[:chain-parallel :start] [:gate :end]]))]




                    [(-> (l/digraph )
                         (l/add-nodes [:chain-linear :start]))
                     (-> (l/digraph )
                         (l/add-nodes [:puzzle :start]))]

                    [(-> (l/digraph )
                         (l/add-edges [[:chain-parallel :start] [:gate :end]]))
                     (-> (l/digraph )
                         (l/add-edges [[:fork :start] [:puzzle 1]]
                                      [[:fork :start] [:puzzle 2]]
                                      [[:fork :start] [:puzzle 3]]
                                      [[:puzzle 1] [:puzzle :end]]
                                      [[:puzzle 2] [:puzzle :end]]
                                      [[:puzzle 3] [:puzzle :end]]))]

                    [(-> (l/digraph )
                         (l/add-nodes [:fork :start]))
                     (-> (l/digraph )
                         (l/add-nodes [:nothing :start]))]

                    [(-> (l/digraph )
                         (l/add-nodes [:fork :start]))
                     (-> (l/digraph )
                         (l/add-edges [[:fork :start] [:puzzle 2]]))]

                    [(-> (l/digraph )
                         (l/add-nodes [:puzzle :start]))
                     (-> (l/digraph )
                         (l/add-edges [[:puzzle :start] [:get-item 2]]))]


                    #_[(-> (l/digraph )
                           (l/add-edges [[:chain :begin] [:gate :end]]))
                       (-> (l/digraph
                            [[:get-potion-quest :begin] [:get-spell-component 2]]
                            [[:get-potion-quest :begin] [:get-spell-component 3]]
                            [[:get-potion-quest :begin] [:get-spell-component 4]]

                            [[:get-spell-component 2] [:mix-ingredients 5]]
                            [[:get-spell-component 3] [:mix-ingredients 5]]
                            [[:get-spell-component 4] [:mix-ingredients 5]]
                            [[:mix-ingredients 5] [:reward :end]]))]



                    #_[(-> (l/digraph )
                           (l/add-nodes [:puzzle :begin]))
                       (-> (l/digraph
                            [[:nothing :begin] [:puzzle 2]]
                            [[:nothing :begin] [:puzzle 3]]
                            [[:nothing :begin] [:puzzle 4]]
                            [[:puzzle 2] [:nothing :end]]
                            [[:puzzle 3] [:nothing :end]]
                            [[:puzzle 4] [:nothing :end]]))]




                    ]
                   (into (map (fn [name]
                                [(-> (l/digraph )
                                     (l/add-nodes [:get-spell-component :component]))
                                 (-> (l/digraph)
                                     (l/add-nodes [name :component]))])
                              [:bone-meal :chicken-feathers :mandrake-root :toad-spittle :fish-oil :river-mud :apple-cores :pinch-of-salt :sea-water :dragon-tears :reptile-scale :troll-spit :cat-hair :swamp-muck :swamp-ooze]))))

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
       (l/nodes graph)))))

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
       (apply l/remove-edges graph))
  )

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

(defn make-graph [rules]
  (loop [graph initial
         shuffled-rules (shuffle rules)]
    
    (let [new-graph (loop [x 0
                           graph graph]
                      (if (< x 10)
                        (recur (inc x)
                               (loop [graph graph
                                      [rule & rules] shuffled-rules]
                                 (if rule
                                   (if (not= graph (apply-rule graph rule))
                                     (apply-rule graph rule)
                                     (recur graph rules))
                                   graph)))
                        graph))]
      (cond
       (every? (complement non-terminal-nodes) (map #(a/attr new-graph % :type) (l/nodes new-graph)))
        new-graph

        (= graph new-graph)
        (throw (Exception. (str "No rules remove non-terminal nodes " (vec (filter non-terminal-nodes (map #(a/attr new-graph % :type) (l/nodes new-graph)))))))

        :else
        (recur 
         new-graph
         (shuffle rules))))))
