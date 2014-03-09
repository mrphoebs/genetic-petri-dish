(ns quilr.core
  (:gen-class)
  ;(:use quilr.quil)
  (:use quilr.dish))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")
  (genetic-algo (list (empty-dish 4))))
