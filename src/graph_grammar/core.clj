(ns graph-grammar.core
  (:require [loom.graph :as l]
            [loom.io :as lio]
            [loom.alg :as alg]
            [loom.attr :as a]
            [loom.derived :as d]))

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


(def rules [
            ;; initial graph
            [
             (-> (l/digraph )
                 (l/add-nodes [:start 1]))
             (-> (l/digraph [[:entrance 1] [:chain 2]]
                            [[:chain 2] [:gate 3]]
                            [[:gate 3] [:mini-boss 4]]
                            [[:mini-boss 4] [:item-quest 5]]
                            [[:item-quest 5] [:test-item 6]]
                            [[:test-item 6] [:chain-final 7]]
                            [[:chain-final 7] [:goal 8]]))]

            ;; make linear chain
            [(-> (l/digraph )
                 (l/add-edges [[:chain 1] [:gate 2]]))
             (-> (l/digraph [[:chain-linear 1] [:chain-linear 3]]
                            [[:chain-linear 3] [:chain-linear 2]]))]

            ;; make final chain
            [(-> (l/digraph )
                 (l/add-edges [[:chain-final 0] [:goal 10]]))

             (-> (l/digraph [[:chain 0] [:test 1]]
                            [[:chain 0] [:gate 5]]
                            
                            [[:test 1] [:hold 2]]
                            [[:test 1] [:key-final 3]]
                            [[:key-final 3] [:lock-final 4]]
                            
                            
                            [[:gate 5] [:lock-final 4]]
                            [[:chain 0] [:hold 6]]
                            [[:lock-final 4] [:boss-level 8]]
                            [[:boss-level 8] [:goal 10]]))]

            #_[(-> (l/digraph )
                 (l/add-edges [[:chain-fork 1] [:item-quest 2]]))
             (-> (l/digraph [[:chain-fork 1] [:chain 3]]
                            [[:chain 3] [:chain-join 4]]
                            [[:chain-fork 1] [:chain 5]]
                            [[:chain 5] [:chain-join 4]]
                            [[:chain-join 4] [:item-quest 2]]))]
            ])

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

(defn find-sequence [path target f]
  (if (seq path)
    (if (= (map first target) (map f  (take (count target) path)))
      (into {}
            (map
             (fn [source dest]
               [(second dest) source ])
             path
             target))
      (find-sequence (rest path) target f))
    nil))

(defn add-nodes-from-output-graph [graph output-graph target-ids]
  (reduce
   (fn [graph [type id]]
     (add-typed-node graph  type (target-ids id)))
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
  (reduce
   (fn [graph path]
     (if-let [matched-nodes (find-sequence path
                                           (first (distinct-paths input))
                                           #(a/attr graph % :type))]
       (let [original-graph graph
             target-ids (into matched-nodes
                                (map (fn [[type n]]
                                       [n  (matched-nodes n (str (java.util.UUID/randomUUID)))]))
                                  (l/nodes output))]
         (-> graph
             (add-nodes-from-output-graph output target-ids)
             (add-edges-from-output-graph output target-ids)
             (remove-edges-from-input input target-ids)
             (add-edges-from-old-nodes output target-ids original-graph)))
       graph))
   graph
   (distinct-paths graph)))


(defn foo
  "I don't do a whole lot."
  []
  (let [graph (reduce
    apply-rule
    
    initial
    rules)
        
        ids (into {} (map vector  (alg/bf-traverse graph (root graph)) (range)))]
    (lio/view
     graph
     
     :node-label #(str (name (or  (a/attr graph % :type) :unknown)) " " (ids %)))))
