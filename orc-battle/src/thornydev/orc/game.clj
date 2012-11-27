(ns thornydev.orc.game
  (:require [thornydev.orc.util :refer [defrecord+defaults
                                        randval]]))

;; TODO: need to apply "functional core, imperative shell" to this
;; so it can be unit tested

;; ---[ global state, types and initializations ]--- ;;

(def player (atom {:health nil
                   :agility nil
                   :strength nil}))

;; ---[ Monster setup and definitions ]--- ;;

(def ^:dynamic *monsters* [])         ;; vector of monsters
(def ^:dynamic *monster-builders* []) ;; vector
(def ^:dynamic *monster-num* 12)      ;; # monsters per fight sequence

(declare monster-dead? player-dead?)

;; do- methods indicate non-pure functions that return
;; void and are invoked to alter state (or just print to *out*
(defprotocol Monster
  "Generic super type for all the monsters in the orc-battle game"
  (do-monster-show [this] "Describes the type of monster")
  (monster-attack [this]
    "Monster attacks player. Modifies player health.
     @return string description of what happened.")
  (monster-hit [this x]
    "Player attacks/hits the monster.
     @params
       mon - monster to 'hit' (subtract health from)
       x - amount of hit
     @returns a new Monster with the new health value")
  )

;; this macro auto-generates a factory function called
;; new-Monster that uses the defaults defined here
(defrecord+defaults Orc [health (randval 10) club-level (randval 8)]
  Monster
  (do-monster-show
   [this]
   (println (format "A wicked Orc with a level %d club and %d health",
                    (:club-level this) (:health this))))
  (monster-attack
   [this]
   (let [pts (randval (:club-level this))]
     (swap! player update-in [:health] - pts)
     (println "An orc swings his club at you and knocks off"
              pts "of your health points"))
   this)

  ;; TODO: can this be made generic?
  (monster-hit
   [this x]
   (let [orc (->Orc (- (:health this) x) (:club-level this))]
     (if (monster-dead? orc)
       (println "You killed the orc!")
       (println "You hit the orc, knocking off" x "health points!"))
     orc
     ))
  )

(defrecord+defaults Hydra [health (randval 10)]
  Monster
  (do-monster-show
   [this]
   (println (format "A malicious hydra with %d heads" (:health this))))

  (monster-attack
   [this]
   (let [x (randval (bit-shift-right (:health this) 1))]
     (print "A hydra attacks you with" x "of its heads!")
     (println "It also grows back one more head.")
     (swap! player update-in [:health] - x)
     (->Hydra (inc (:health this)))
     ))

  ;; TODO: can this be made generic?
  (monster-hit
   [this x]
   (let [hyd (->Hydra (- (:health this) x))]
     (if (monster-dead? hyd)
       (println "The corpse of the fully decapitated hydra falls to the floor.")
       (println "You lopped" x "of the hydra's heads!")
       )
     hyd))
  )

(comment
  (binding [*monster-num* 12]
    (init-player)
    (let [orc (new-Orc)
          hyd (new-Hydra)]
      (do-monster-show orc)
      (do-monster-show hyd)
      (monster-attack orc)
      (monster-attack orc)
       
      [@player]
      ))
  )


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


(defn monster-dead? [mon]
  (<= (:health mon) 0))

;; TODO: does this need to go into the Monster protocol??
;; (defn monster-hit
;;   "@params
;;      mon - monster to 'hit' (subtract health from)
;;      x - amount of hit
;;    @returns a new Monster with the new health value
;;    Note that this fn will not modify the global list of monsters.
;;    This fn returns a new monster that will need to slide into the
;;    global vector of monsters"
;;   [mon x]
;;   ;; (->Monster (- (:health mon) x))
;;   )

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
  [nmonsters]
  (binding [*monster-builders* [new-Orc new-Hydra]]
    (binding [*monster-num* nmonsters]
      (init-monsters)
      (init-player)
      (game-loop)
      (cond
       (player-dead?)   (println "You have been killed. Game over.")
       (monsters-dead?) (println "Congratulations! You have vanquished your foes."))))
  )
