(ns thornydev.wumpus.bench
  (:require [thornydev.wumpus.game :refer :all]
            [criterium.core :refer [bench]]))



(defn run-bench []
  (bench (make-city-edges))
  )

