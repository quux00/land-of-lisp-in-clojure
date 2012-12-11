(ns thornydev.doomdice.game)

(def ^:dynamic *num-players* 2)
(def ^:dynamic *max-dice* 3)
(def ^:dynamic *board-size* 2)
(def ^:dynamic *board-hexnum* (* *board-size* *board-size*))

(defn board-vector [lst]
  (vec lst))

;; TODO: add way to get known board for testing
(defn gen-board []
  (board-vector
   (for [n (range *board-hexnum*)]
     (vector (rand-int *num-players*) (inc (rand-int *max-dice*))))))

(defn player-letter [n] (char (+ 97 n)))

(defn do-draw-board [board]
  (doseq [y (range *board-size*)]
    (do
      (println)
      (dotimes [_ (- *board-size* y)] (print "  "))
      (doseq [x (range *board-size*)]
        (let [hex (nth board (+ x (* *board-size* y)))]
          (print (str (player-letter (first hex)) "-" (second hex) " ")))))))

(defn game-tree
  "@params
    board - ???
    player - integer (eg., 0 for white, 1 for black)
    spare-dice - integer: number of dice WHERE??
    first-move? - bool: whether is player's first move for this turn
  @return ???"
  [board player spare-dice first-move?]
  (vector player
          board
          (add-passing-move board
                            player
                            spare-dice
                            first-move?
                            (attacking-moves board player spare-dice))))

;; TODO: data all the things => something to try here?
(defn add-passing-move
  "@params
    board - ???
    player - integer (eg., 0 for white, 1 for black)
    spare-dice - integer: number of dice WHERE??
    first-move? - bool: whether is player's first move for this turn
    moves - ??? (list?) of moves : probably attacking moves ???
  @return ???"
  [board player spare-dice first-move? moves]
  (if first-move?
    moves            ;; cannot pass on first-move, so return attack-moves
    (cons (list nil  ;; desc of the move => nil means "passing"
                (game-tree (add-new-dice board player (dec spare-dice))
                           (mod (inc player) *num-players*)  ;; change to next player
                           0                                 ;; no spare dice
                           true))                            ;; first-move
          moves)))

(comment
  (gen-board)
  (player-letter 0)
  (do-draw-board (gen-board))
  )
