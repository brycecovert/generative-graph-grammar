(ns graph-grammar.main
  (:require [graph-grammar.core :as c])
  (:gen-class))

(defn -main
  "I don't do a whole lot."
  []
  (println "Generating a single level")
  (dotimes [x 10] (c/view (c/make-graph c/single-puzzle-rules)))
  (println "Done.")
  (Thread/sleep 5000)
  (System/exit 0))
