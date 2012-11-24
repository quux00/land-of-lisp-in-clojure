(ns thornydev.wumpus.game
  (:require [thornydev.wumpus.graphing :refer [ugraph->svg]]))

(def city-nodes (atom nil))
(def city-edges (atom nil))

;; TODO: these three should probably be part of a single atom
(def player-pos (ref nil))
(def visited-nodes (ref #{}))
(def player-status (atom :in-progress))

;; TODO: no point in making these dynamic unless you write a game-repl
;;       loop in which they can be dynamically bound
(def ^:dynamic *node-num* 30)
(def ^:dynamic *edge-num* 45)
(def ^:dynamic *worm-num* 3)
(def ^:dynamic *cop-odds* 15)  ; 1/15 change of police roadblock on an edge


(defn filterb
  "Filter varient that returns nil (a falsy value) if
   nothing passes the filter.  If anything is retained
   by the filter, than it returns what filter normally does."
  [pred coll]
  (let [rt (filter pred coll)]
    (if (empty? rt)
      nil
      rt)))

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

(defn get-connected
  "@return set of all nodes that are connected in the edge-list"
  [edge-list]
  (set (mapcat identity edge-list)))

(defn find-islands
  "@return seq/list of nodes not connected to other nodes"
  [node-list edge-list]
  (filter (complement (get-connected edge-list)) node-list))

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

;; this correlates to edges-to-alist in the LOL book
(defn edges-map [edges-vec edges-with-cops]
  (let [cops-set (set edges-with-cops)
        adorn-edge (fn [edge]
                     (if (cops-set (sort edge))
                       [[(second edge) :COPS]]
                       [[(second edge)]]))]
    (apply merge-with (fn [& args] (vec (apply concat args)))
           (map #(assoc {} (first %) (adorn-edge %)) edges-vec))))

(defn add-cops
  "picks a random subset of edges (form [x y]) that
   will have cops on the edge (not on the node)
   @return seq of edges (eg., ([3 14] [6 8])"
  [edges-vec]
  (map sort
       (filter (fn [e] (zero? (rand-int *cop-odds*))) edges-vec)))

(defn make-city-edges []
  (let [all-edges       (connect-all-islands (make-edge-vec))
        edges-with-cops (add-cops all-edges)]
    (edges-map all-edges edges-with-cops)))

(defn neighbors
  "@params
     node: integer representing the node
     edges-map: map of edges created by make-city-edges
   @return set of neighbors for +node+"
  [node edges-map]
  (set (map first (get edges-map node))))


(defn within-one [nd1 nd2 edges-map]
  (boolean
   ((neighbors nd1 edges-map) nd2)))


(defn within-two [nd1 nd2 edges-map]
  (or (within-one nd1 nd2 edges-map)
      (boolean
       (some #(within-one % nd2 edges-map)
             (neighbors nd1 edges-map)))))


(defn make-city-nodes [edges-map]
  (let [wumpus (rand-node)
        glow-worms (->> (range *node-num*)
                        (reduce (fn [st _] (conj st (rand-node))) #{})
                        shuffle
                        (take 3)
                        set)]
    (vec
     (for [k (keys edges-map)]
       (vec (concat
             [k]
             (cond
              (= k wumpus) [:WUMPUS]
              (within-two k wumpus edges-map) [:BLOOD!]
              :else [])
             (cond
              (glow-worms k) [:GLOW-WORM]
              (some #(within-one k % edges-map) glow-worms) [:LIGHTS!]
              :else [])
             (when (some #(first (rest %)) (get edges-map k))
               [:SIRENS!])))))))

(defn find-empty-node []
  (if-let [empty-nd (first
                     (filter #(= 1 (count %))
                             (shuffle @city-nodes)))]
    (first empty-nd)
    (throw (IllegalStateException. "No empty nodes available. Try another game configuration."))))


(defn draw-city []
  (ugraph->svg "wumpus" @city-edges @city-nodes))


(defn known-city-nodes []
  (let [visited (keep (fn [nd]
                        (when (@visited-nodes (first nd))
                          (if (= @player-pos (first nd))
                            (conj nd "*")
                            nd)))
                      @city-nodes)
        visited-idxs-set (set (map first visited))
        visible (for [seen-node visited
                      newnd (@city-edges (first seen-node))
                      :when (not (visited-idxs-set (first newnd)))]
                  (vector (first newnd) "?"))]
    (vec (concat visited visible))))


(defn known-city-edges []
  (let [remove-cops (fn [v]
                      (mapv #(vec (take 1 %)) v))]
    (reduce #(assoc % %2 (remove-cops (@city-edges %2))) {} @visited-nodes)))


(defn draw-known-city []
  (ugraph->svg "known-city" (known-city-edges) (known-city-nodes)))

;; ---[ user invokable helpers ]--- ;;

(declare status)

(defn game-over [win-lose msg]
  (reset! player-status win-lose)
  (println msg))


(defn do-move [pos edge type]
  (let [node (first (filter #(#{pos} (first %)) @city-nodes))
        has-worm (and (some #{:GLOW-WORM} node)
                      (not (@visited-nodes pos)))]

    (dosync
     (ref-set player-pos pos)
     (alter visited-nodes conj pos))

    (draw-known-city)

    (cond
     (some #{:COPS} edge) (game-over :lose "You ran into the cops. Game over.")
     (some #{:WUMPUS} node) (if (= type :charge)
                              (game-over :win "You found and terminated the wumpus!")
                              (game-over :lose "The wumpus has wumped you with his AK47."))
     (= type :charge) (println "You wasted your last bullet. Game over.")
     has-worm (let [new-pos (rand-node)]
                  (println "You ran to a Glow Worm Gang! You're now at" new-pos)
                  (do-move new-pos nil :move))
     :else (status))))

(defn move-to
  [pos type]
  (case @player-status
    :lose (println "You cannot move as you have died or been captured.")
    :win  (println "No moves left - you alread won.")
    :in-progress (if-let [edge-ls (filterb #(#{pos} (first %))
                                           (@city-edges @player-pos))]
                   (do-move pos (first edge-ls) type)
                   (println "You cannot go to that location from where you are!"))
    ))

;; ---[ user invokable 'commands' ]--- ;;

(defn status []
  (println "Current position:" @player-pos)
  (println "Visted nodes:" @visited-nodes)
  (println "Known roads: " (known-city-edges)))


(defn new-game []
  (reset! city-edges (make-city-edges))
  (reset! city-nodes (make-city-nodes @city-edges))
  (dosync
   (ref-set player-pos (find-empty-node))
   (ref-set visited-nodes #{@player-pos}))
  (draw-city)
  (draw-known-city))

(defn walk [pos]
  (move-to pos :walk))

(defn charge [pos]
  (move-to pos :charge))





;; ---[ data structure documentation ]--- ;;

(comment
  ;; this is an example of what city-edges looks like
  {1 [[3] [7] [12] [29] [30]], 2 [[17]], 3 [[9] [1]], 4 [[16]], 5 [[28]], 6 [[8] [16] [27]], 7 [[9] [18] [1]], 8 [[26] [28] [6]], 9 [[12 :COPS] [13] [18] [23] [30 :COPS] [3] [7]], 10 [[14 :COPS]], 11 [[22]], 12 [[25] [1] [9 :COPS]], 13 [[24] [27] [9]], 14 [[21] [24] [25] [10 :COPS]], 15 [[17] [24] [30]], 16 [[23 :COPS] [27] [30 :COPS] [4] [6]], 17 [[2] [15]], 18 [[28] [7] [9]], 19 [[22]], 20 [[30]], 21 [[25] [14]], 22 [[11] [19]], 23 [[9] [16 :COPS]], 24 [[28] [13] [14] [15]], 25 [[29] [30] [12] [14] [21]], 26 [[8]], 27 [[6] [13] [16]], 28 [[5] [8] [18] [24]], 29 [[1] [25]], 30 [[1] [9 :COPS] [15] [16 :COPS] [20] [25]]})


(comment
  ;; exmaple of city-nodes
  [[1] [2 :LIGHTS! :SIRENS!] [3 :SIRENS!] [4 :BLOOD! :SIRENS!] [5 :SIRENS!] [6 :WUMPUS :GLOW-WORM :SIRENS!] [7] [8] [9 :LIGHTS!] [10 :LIGHTS! :SIRENS!] [11 :SIRENS!] [12] [13 :BLOOD! :LIGHTS!] [14 :GLOW-WORM] [15] [16 :BLOOD! :LIGHTS!] [17 :BLOOD! :LIGHTS!] [18 :BLOOD!] [19 :SIRENS!] [20 :SIRENS!] [21] [22] [23 :SIRENS!] [24 :BLOOD! :GLOW-WORM] [25] [26 :BLOOD! :SIRENS!] [27 :BLOOD! :LIGHTS! :SIRENS!] [28] [29 :BLOOD! :LIGHTS! :SIRENS!] [30]]
  )
