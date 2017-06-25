(ns graph-grammar.main
  (:require [graph-grammar.core :as c])
  (:gen-class))

(defn -main
  "I don't do a whole lot."
  []
  (println "Generating a single level")
  (c/view (c/make-graph))
  (println "Done.")
  (Thread/sleep 5000)
  (System/exit 0))
