(ns thornydev.orc.game
  (:require [thornydev.orc.util :refer [defrecord+defaults
                                        randval]]))

;; TODO: need to apply "functional core, imperative shell" to this
;; so it can be unit tested

;; ---[ global state, types and initializations ]--- ;;

(def player (atom {:health nil
                   :agility nil
                   :strength nil}))

(def ^:dynamic *monsters* [])         ;; vector of monsters
(def ^:dynamic *monster-builders* []) ;; vector
(def ^:dynamic *monster-num* 12)      ;; # monsters per fight sequence

;; this macro auto-generates a factory function called
;; new-Monster that uses the defaults defined here
(defrecord+defaults Monster [health (randval 10)])

(defn init-player []
  (swap! player merge {:health 30, :agility 2, :strength 30}))

(defn init-monsters []
  
  )


;; ---[ player functions ]--- ;;
(defn player-dead? []
  (<= (:health @player) 0))

(defn show-player
  "Meta: imperative.
   Prints current status to *out*."
  []
  (println)
  (println "You are a valiant knight with health of" (:health @player)
           ", an agility of" (:agility @player)
           ", and a strength of" (:strength @player)))

(defn stab-monster [monster])


;; TODO: these need to be in a protocol or multi-method
(defn monster-dead? [])
(defn monster-attack [])

(defn monster-hit
  "@params
     mon - monster to 'hit' (subtract health from)
     x - amount of hit
   @returns a new Monster with the new health value
   Note that this fn will not modify the global list of monsters.
   This fn returns a new monster that will need to slid into the
   global vector of monsters"
  [mon x]
  (->Monster (- (:health mon) x)))

(defn get-monster-choice
  "Looks up the monster from the *monster* vector
   by the idx passed in
   @return the monster referenced by the index as long
           as it is not dead
   @throws IllegalArgumentException if the idx is out of
           bounds for the monster array or the monster is dead."
  [^Integer x]
  (if (or (>= x 1) (<= x *monster-num*))
    (throw (IllegalArgumentException. "That is not a valid monster number."))
    (let [mon (nth *monsters* (dec x))]
      (if (monster-dead? mon)
        (throw (IllegalArgumentException. "That monster is already dead."))
        mon))))


(defn pick-monster
  "Meta: imperative
   Queries user for which monster to target and
   delegates to the functional get-monster-choise fn"
  []
  (println)
  (println "Monster #:")
  (flush)
  (let [input (read-line)]
    (if (re-matches #"\d+" input)
      (try
        (get-monster-choice (read-string input))
        (catch IllegalArgumentException e
          (println (.getMessage e))
          (pick-monster))
        )
      )
    )
  )

(defn player-attack []
  "Meta: imperative
   Queries user for type of attack and then delegates
   to the functional core method do-player-attack"
  (println)
  (println "Attack style: [s]tab [d]ouble swing [r]oundhouse: ")
  (flush)
  (loop [input (read-line)]
    (case input
      "s" (stab-monster (pick-monster))
      ;; "d" (do-player-attack :double-swing)
      ;; "r" (do-player-attack :roundhouse)
      (do (println "Input not recognized")
          (recur (read-line)))
      )
    )
  )


;; ---[ monster functions ]--- ;;
(defn init-monsters [])
(defn show-monsters [])
(defn monsters-dead? []
  true)


;; TODO: tie this into game-repl from wizards-adventure?
(defn game-loop
  ""
  []
  (when-not (or (player-dead?) (monsters-dead?))
    (show-player)
    (dotimes [k (:agility @player)]
      (if-not (monsters-dead?)
        (show-monsters)
        (player-attack)))
    (println)
    (doseq [mon *monsters*]
      (when-not (monster-dead? mon)
        (monster-attack mon)))
    (recur)
    ))











;; ---[ main game fns ]--- ;;

(defn orc-battle
  "start the game"
  []
  (init-monsters)
  (init-player)
  (game-loop)
  (cond
   (player-dead?)   (println "You have been killed. Game over.")
   (monsters-dead?) (println "Congratulations! You have vanquished your foes."))
  )
