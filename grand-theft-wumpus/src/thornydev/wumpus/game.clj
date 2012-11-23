(ns thornydev.wumpus.game)

(def city-nodes nil)
(def city-edges nil)
(def visited-nodes (atom nil))

(def ^:dynamic *node-num* 30)
(def ^:dynamic *edge-num* 45)
(def ^:dynamic *worm-num* 3)
(def ^:dynamic *cop-odds* 15)  ; 1/15 change of police roadblock at a node

(defn random-node
  "Get integer position of a random node, starting
   index at 1 (range 1 .. *node-num** inclusive)"
  []
  (inc (rand-int *node-num*)))
