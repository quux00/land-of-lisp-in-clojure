(ns thornydev.wumpus.game)

(def city-nodes nil)
(def city-edges nil)
(def visited-nodes (atom nil))

(def ^:dynamic *node-num* 30)
(def ^:dynamic *edge-num* 45)
(def ^:dynamic *worm-num* 3)
(def ^:dynamic *cop-odds* 15)  ; 1/15 change of police roadblock at a node

(defn all-nodes []
  (range 1 (inc *node-num*)))

(defn rand-node
  "Get integer position of a random node, starting
   index at 1 (range 1 .. *node-num** inclusive)"
  []
  (inc (rand-int *node-num*)))

(defn edge-pair [x y]
  (when-not (= x y)
    [[x y] [y x]]))

(defn make-edge-vec []
  (->> (repeatedly #(edge-pair (rand-node) (rand-node)))
       (take *edge-num*)
       (apply concat)
       set
       vec))

(comment
  (count (make-edge-vec)))

(defn get-connected
  "@return set of all nodes that are connected in the edge-list"
  [edge-list]
  (set (mapcat identity edge-list)))

(comment
  (get-connected (make-edge-vec)))

(defn find-islands
  "@return seq/list of nodes not connected to other nodes"
  [node-list edge-list]
  (filter (complement (get-connected edge-list)) node-list))

(comment
  (find-islands (range 1 *node-num*) (make-edge-vec))
  )

(defn connect-all-islands
  "@param
    connected-nodes that are already connected in edge-list
    edge-vec: created via make-edge-vec"
  [edge-vec]
  (let [connected-nodes (vec (get-connected edge-vec))]
    (concat
     (mapcat (fn [island]
               (edge-pair
                island
                (nth connected-nodes (rand-int (count connected-nodes)))))
             (find-islands (all-nodes) edge-vec))
     edge-vec)))


(comment
  (let [edgevec          (make-edge-vec)
        islands          (find-islands (all-nodes) edgevec)]
    {:edges edgevec
     :islands islands
     :connect-all (connect-all-islands edgevec)
     }
    )
  )


;; this correlates to edges-to-alist in the LOL book
(defn edges-map [edges-vec edges-with-cops]
  (let [cops-set (set edges-with-cops)
        adorn-edge (fn [edge]
                     (if (cops-set (sort edge))
                       [[(second edge) :cops]]
                       [[(second edge)]]))]
    (apply merge-with (fn [& args] (vec (apply concat args)))
           (map #(assoc {} (first %) (adorn-edge %)) edges-vec))))

;; (defn add-cops-1 [edges-map]
;;   (loop [keys (keys edges-map) marked []]
;;     (if (empty? keys)
;;       marked
;;       (do
;;         (if (zero? (rand-int *cop-odds*))
;;           (recur (rest keys) (conj marked (first keys)))
;;           (recur (rest keys) marked)
;;           )
;;         )
;;       )
;;     ))

(defn add-cops
  "picks a random subset of edges (form [x y]) that
   will have cops on the edge (not on the node)
   @return seq of edges (eg., ([3 14] [6 8])"
  [edges-vec]
  (map sort
       (filter (fn [e] (zero? (rand-int *cop-odds*))) edges-vec)))

(defn make-city-edges []
  (let [all-edges       (connect-all-islands (make-edge-vec))
        edges-with-cops (add-cops all-edges)
        final-map       (edges-map all-edges edges-with-cops)]
    ;; for debugging => TODO: remove later
    {:final final-map,
     :cop-edges edges-with-cops}
    final-map
    )
  )

(comment
  (type (make-city-edges))
  )
