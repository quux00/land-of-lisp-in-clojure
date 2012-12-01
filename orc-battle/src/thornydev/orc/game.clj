(ns thornydev.orc.game
  (:require [thornydev.orc.util :refer [randval]]
            [thornydev.orc.monsters :refer :all]))

;; ---[ global state, types and initializations ]--- ;;

(def player (atom {:health nil
                   :agility nil
                   :strength nil}))

(defn create-default-monsters
  "This creates a default set of monsters to have a known
   defined set for testing or basic play. If you want just
   this set, either do not call init-monsters or call it:
   (init-monsters (create-default-monsters)"
  []
  [(atom (->Orc 6 5)) (atom (->Brigand 8))
   (atom (->Hydra 7)) (atom (->SlimeMold 5 7))] )

(def ^:dynamic *monster-builders* []) ;; vector
(def ^:dynamic *monsters* (create-default-monsters))
(def ^:dynamic *monster-num* (count *monsters*)) ;; # monsters per fight seq

(defn init-player []
  (swap! player merge {:health 30, :agility 30, :strength 30}))

(defn player-dead? []
  (<= (:health @player) 0))

(defn create-random-monsters
  "Creates a random vector of monsters based on *monster-num*
   and *monster-builders* being set before this fn is called.
   It does not modify *monsters*. It returns a vector of monsters
   (of size *monster-num*). It is intended to be called in a
   binding form that sets *monsters*."
  []
  (mapv (fn [_]
          (atom ((nth *monster-builders*
                      (rand-int (count *monster-builders*))))))
        (range *monster-num*)))


;; ---[ player functions ]--- ;;
(defn show-player
  "Meta: imperative.
   Prints current status to *out*."
  []
  (println)
  (println "You are a valiant knight with health of"
           (str (:health @player)
                ", an agility of " (:agility @player)
                ", and a strength of " (:strength @player))))

(defn get-monster-choice
  "Looks up the monster from the monster atom
   by the idx passed in (using 1-based indexing)
   @return the atom for the monster referenced by the index 
           as long as it is not dead
   @throws IllegalArgumentException if the idx is out of
           bounds for the monster array or the monster is dead."
  [^Integer x]
  (if (or (< x 1) (> x *monster-num*))
    (throw (IllegalArgumentException. "That is not a valid monster number."))
    (let [mon (nth *monsters* (dec x))]
      (if (monster-dead? @mon)
        (throw (IllegalArgumentException. "That monster is already dead."))
        mon))))


(defn pick-monster
  "Meta: imperative
   Queries user for which monster to target and
   delegates to the functional get-monster-choice fn"
  []
  (println)
  (println "Monster #: ")
  (flush)
  (let [input (read-line)]
    (if (re-matches #"\d+" input)
      (try
        (get-monster-choice (read-string input))
        (catch IllegalArgumentException e
          (println (.getMessage e))
          (pick-monster))
        ))))


(defn stab-player-attack []
  (let [mon (pick-monster)]
    (reset! mon
            (monster-hit
             @mon
             (+ 2 (randval (bit-shift-right (:health @player) 1)))))))



(defn double-swing-player-attack []
  (let [x (randval (/ (:strength @player) 6))]
    (println "Your double swing has a strength of" x)
    ;; once this works, refactor to be DRY (loop-recur?)
    (let [mon (pick-monster)]
      (reset! mon (monster-hit @mon x)))
    (when-not (monsters-dead? *monsters*)
      (let [mon (pick-monster)]
        (reset! mon (monster-hit @mon x)))
      )))

(defn roundhouse-player-attack []
  (dotimes [_ (inc (randval (/ (:strength @player) 3)))]
    (when-not (monsters-dead? *monsters*)
      (let [mon (random-monster *monsters*)]
        (reset! mon (monster-hit @mon 1)))
      )))

(defn player-attack []
  "Meta: imperative
   Queries user for type of attack and then delegates
   to the functional core method do-player-attack"
  (println)
  (println "Attack style: [s]tab [d]ouble swing [r]oundhouse: ")
  (flush)
  (loop [input (read-line)]
    (case input
      "s" (stab-player-attack)
      "d" (double-swing-player-attack)
      "r" (roundhouse-player-attack)
      (do (println "Input not recognized")
          (recur (read-line)))
      )
    )
  )


;; ---[ monster functions ]--- ;;
(defn show-monsters
  "Meta: imperative"
  []
  (println "Your foes:")
  (doseq [[idx mon] (map vector (iterate inc 1) *monsters*)]
    (print "  " (str idx ": "))
    (if (monster-dead? @mon)
      (println "**dead**")
      (do (print "Health =" (:health @mon) " ")
          (do-monster-show @mon))
      )))


(defn pause []
  (print "Press Enter to Continue: ")
  (flush)
  (read-line))

(defn game-loop
  "Main game loop.
   Ends when either the player dies or when all the monsters are dead."
  []
  (when-not (or (player-dead?) (monsters-dead? *monsters*))
    (show-player)
    (pause)
    (dotimes [k (inc (/ (max 0 (:agility @player)) 15))]
      (when-not (monsters-dead? *monsters*)
        (show-monsters)
        (player-attack)))
    (println)
    (doseq [mon *monsters*]
      (when-not (monster-dead? @mon)
        (reset! mon (monster-attack @mon player))))
    (recur)))


;; ---[ main game fns ]--- ;;

(defn orc-battle
  "start the game"
  [nmonsters]
  (binding [*monster-builders* [new-Orc new-Hydra new-SlimeMold new-Brigand]]
    (binding [*monster-num* nmonsters]
      (binding [*monsters* (create-random-monsters)]
        (init-player)
        (game-loop)
        (cond
         (player-dead?)
         (println "You have been killed. Game over.")

         (monsters-dead? *monsters*)
         (println "Congratulations! You have vanquished your foes.")))))
  )
