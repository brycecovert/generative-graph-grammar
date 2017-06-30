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
   (let [g (l/add-nodes g id)]
     (cond
       (keyword? type)
       (a/add-attr-to-nodes g :type type [id])

       (map? type)
       (reduce-kv
        (fn [g k v]
          (a/add-attr-to-nodes g k v [id]))
        g
        type
        )

       :else
        g))))

(defn add-typed-edge
  ([g type1 type2]
   (add-typed-edge g type1 (str (java.util.UUID/randomUUID)) type2 (str (java.util.UUID/randomUUID))))
  
  ([g type1 id1 type2 id2]
   (-> g
       (add-typed-node type1 id1)
       (add-typed-node type2 id2)
       (l/add-edges [id1 id2]))))

(def initial (-> (l/digraph)
                 (add-typed-node :start )))


(def non-terminal-nodes
  #{#_:gate :chain-linear :chain-parallel :chain :hook :fork :start :chain-final :get-spell-component :initial-task :init-lock :init-key})

(def single-puzzle-rules
  {:initial [(-> (l/digraph)
                 (add-typed-node :start 1))
             (-> (l/digraph)
                 (add-typed-edge :begin 1 :task 2)
                 (add-typed-edge :task 2 :task 3)
                 (add-typed-edge :task 3 :task 4)
                 (add-typed-edge :task 4 :task 5)
                 (add-typed-edge :task 5 :task 6)
                 (add-typed-edge :task 6 :task 7)
                 (add-typed-edge :task 7 :task 8)
                 (add-typed-edge :task 8 :end 9))]
   :add-task [(-> (l/digraph)
                  (add-typed-edge :task 1 :task 2))
              (-> (l/digraph)
                  (add-typed-edge :task 1 :task 3)
                  (add-typed-edge :task 3 :task 2))]

   ;; rule 1
   :generate-lock [(-> (l/digraph)
                       (add-typed-edge nil 1 :task 2)
                       (add-typed-edge :task 2 nil 4))
                   (-> (l/digraph)
                       (add-typed-edge nil 1 :init-key 3)
                       (add-typed-edge nil 1 :init-lock 2)
                       (add-typed-edge :init-key 3 :init-lock 2)
                       (add-typed-edge :init-lock 2 nil 4))]

   :specify-lock-1 [(-> (l/digraph)
                        (add-typed-edge :init-key :init-key :init-lock :init-lock))
                    (-> (l/digraph)
                        (add-typed-edge {:type :key :color :red} :init-key {:type :lock :color :red} :init-lock))]

   :specify-lock-2 [(-> (l/digraph)
                        (add-typed-edge :init-key :init-key :init-lock :init-lock))
                    (-> (l/digraph)
                        (add-typed-edge {:type :key :color :blue} :init-key {:type :lock :color :blue} :init-lock))]
   
   :specify-lock-3 [(-> (l/digraph)
                        (add-typed-edge :init-key :init-key :init-lock :init-lock))
                    (-> (l/digraph)
                        (add-typed-edge {:type :key :color :green} :init-key {:type :lock :color :green} :init-lock))]

   :specify-lock-4 [(-> (l/digraph)
                        (add-typed-edge :init-key :init-key :init-lock :init-lock))
                    (-> (l/digraph)
                        (add-typed-edge {:type :key :color :orange} :init-key {:type :lock :color :orange} :init-lock))]

   :specify-lock-5 [(-> (l/digraph)
                        (add-typed-edge :init-key :init-key :init-lock :init-lock))
                    (-> (l/digraph)
                        (add-typed-edge {:type :key :color :magenta} :init-key {:type :lock :color :magenta} :init-lock))]

   :specify-lock-6 [(-> (l/digraph)
                        (add-typed-edge :init-key :init-key :init-lock :init-lock))
                    (-> (l/digraph)
                        (add-typed-edge {:type :key :color :aqua} :init-key {:type :lock :color :aqua} :init-lock))]

   :specify-lock-7 [(-> (l/digraph)
                        (add-typed-edge :init-key :init-key :init-lock :init-lock))
                    (-> (l/digraph)
                        (add-typed-edge {:type :key :color :yellow} :init-key {:type :lock :color :yellow} :init-lock))]

   :specify-lock-8 [(-> (l/digraph)
                        (add-typed-edge :init-key :init-key :init-lock :init-lock))
                    (-> (l/digraph)
                        (add-typed-edge {:type :key :color :brown} :init-key {:type :lock :color :brown} :init-lock))]

   :specify-lock-9 [(-> (l/digraph)
                        (add-typed-edge :init-key :init-key :init-lock :init-lock))
                    (-> (l/digraph)
                        (add-typed-edge {:type :key :color :black} :init-key {:type :lock :color :black} :init-lock))]


   ;; rule 2
   :move-lock [(-> (l/digraph)
                   (add-typed-edge nil 1 :lock 4)
                   (add-typed-edge nil 2 nil 3)
                   (add-typed-edge nil 3 :lock 4)
                   (add-typed-edge :lock 4 nil 5))
               (-> (l/digraph)
                   (add-typed-edge nil 1 :lock 4)
                   (add-typed-edge nil 2 nil 3)
                   (add-typed-edge nil 2 :lock 4)
                   (add-typed-edge nil 3 :lock 4)
                   (add-typed-edge :lock 4 nil 5))]


   ;; rule 4
   :move-key [(-> (l/digraph)
                  (add-typed-edge nil 1 :key 2)
                  (add-typed-edge :key 2 :lock 3)
                  (add-typed-edge :lock 3 :task 4)
                  (add-typed-edge :task 4 nil 5))
              (-> (l/digraph)
                  (add-typed-edge nil 1 :task 4)
                  (add-typed-edge :task 4 :key 2)
                  (add-typed-edge :key 2 :lock 3)
                  (add-typed-edge :lock 3 nil 5))]

   ;; rule 5
   :duplicate-key [(-> (l/digraph)
                       (add-typed-edge :task :task :lock :lock)
                       (add-typed-edge :key :key :lock :lock))
                   (-> (l/digraph)
                       (add-typed-edge :task :task :key :new-key)
                       (add-typed-edge :task :task :lock :lock)
                       (add-typed-edge :key :key :lock :lock)
                       (add-typed-edge :key :new-key :lock :lock))]

   ;; rule 6
   :duplicate-lock [(-> (l/digraph)
                        (-> (l/digraph)
                            
                            (add-typed-edge :key :key :lock :lock)
                            (add-typed-edge :lock :lock nil 1)
                            (add-typed-edge :nil 1 :task :task)))
                    (-> (l/digraph)
                        (add-typed-edge :key :key :lock :lock)
                        (add-typed-edge :lock :lock nil 1)
                        (add-typed-edge :nil 1 :lock :task)
                        (add-typed-edge :key :key :lock :task))]})

(def recipe [[[:initial] 1 1]
             [[:add-task] 1 5]
             [[:generate-lock] 1 5]
             [[:specify-lock-1 :specify-lock-2 :specify-lock-3 :specify-lock-3  :specify-lock-4 :specify-lock-5 :specify-lock-6 :specify-lock-7 :specify-lock-8 :specify-lock-9] 5 5]
             [[:move-lock :move-key #_:duplicate-lock #_:duplicate-key] 10 40]])




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
                  [(assignments f) (assignments t)])
                (l/edges subgraph))))
     assignments]
    (let [is-unassigned? (complement (set (keys assignments)))
          next-assignment (->> (l/nodes subgraph)
                               (filter is-unassigned?)
                               first)
          next-assignment-type (a/attr subgraph next-assignment :type)]
      
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
   (fn [graph target-id]
     (let [attributes (a/attrs output-graph target-id)]
       (add-typed-node graph attributes (target-ids target-id))))
   graph
   (l/nodes output-graph)))

(defn add-edges-from-output-graph [graph output-graph target-ids]
  (reduce
   (fn [graph [from-id to-id]]
     (l/add-edges graph [(target-ids from-id) (target-ids to-id)]))
   graph
   (l/edges output-graph)))

(defn remove-edges-from-input [graph input target-ids]
  (->> (l/edges input)
       (map (fn [[from to]] [(target-ids from) (target-ids to)]))
       (apply l/remove-edges graph)) 
  )

(defn apply-rule [graph [input output]]
  (if-let [matched-nodes (search-for-subgraph graph input)]
    (let [original-graph graph
          target-ids (into matched-nodes
                           (map (fn [n]
                                  [n  (matched-nodes n (str (java.util.UUID/randomUUID)))]))
                           (l/nodes output))]
      (-> graph
          (add-nodes-from-output-graph output target-ids)
          (remove-edges-from-input input target-ids)
          (add-edges-from-output-graph output target-ids)))
    graph))

(defn view [graph]
  (let [ids (into {} (map vector  (alg/bf-traverse graph (root graph)) (range)))]
    (lio/view
     graph
     :node-label #(str (name (or  (a/attr graph % :type) :unknown)) " " (ids %) (a/attr graph % :color)))))

(defn make-graph [recipe rules]
  (reduce
   (fn [graph [rule-keys min max]]
     (let [random-iterations (+ min (rand-int (- max min)))]
       (loop [x 0
              graph graph]
         (let [new-graph (apply-rule graph ((rand-nth rule-keys) rules))]
           #_(println "recurring " x random-iterations graph new-graph)
           (if (or #_(and (= random-iterations Integer/MAX_VALUE) (not= new-graph graph))
                   (< x random-iterations))
             
             (recur (inc x)
                    new-graph)
             new-graph))))) 
   initial 
   recipe))
