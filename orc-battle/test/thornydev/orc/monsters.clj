(ns thornydev.orc.monsters
  (:require [thornydev.orc.util :refer [defrecord+defaults
                                        randval]]))

(declare monster-dead?)

(defn generic-monster-hit
  "General monster hit fn.
   Note that this fn does NOT modify the monsters atom.
   Instead it returns a new monster that the caller needs to
   modify the monsters atom to keep game state consistent"
  [mon x]
  (let [mon2 (into mon {:health (- (:health mon) x)})
        mon-type (second (re-find #"\.(\w+)$" (str (type mon))))]
    (if (monster-dead? mon2)
      (println (str "You killed the " mon-type "!"))
      (println "You hit the" (str mon-type ", knocking off") x "health points!"))
    mon2))


;; do- methods indicate non-pure functions that return
;; void and are invoked to alter state (or just print to *out*
(defprotocol Monster
  "Generic super type for all the monsters in the orc-battle game"
  (do-monster-show [this] "Describes the type of monster")
  (monster-attack [this player]
    "Monster attacks player. Modifies player health.
     @return string description of what happened.")
  (monster-hit [this x]
    "Player attacks/hits the monster.
     @params
       mon - monster to 'hit' (subtract health from)
       x - amount of hit
     @returns a new Monster with the new health value")
  )

;; ---[ Orc ]--- ;;
;; this macro auto-generates a factory function called
;; new-Monster that uses the defaults defined here
(defrecord+defaults Orc [health (randval 10) club-level (randval 8)]
  Monster
  (do-monster-show
   [this]
   (println (format "A wicked Orc with a level %d club",
                    (:club-level this))))
  (monster-attack
   [this player]
   (let [pts (randval (:club-level this))]
     (swap! player update-in [:health] - pts)
     (println "An orc swings his club at you and knocks off"
              pts "of your health points"))
   this)

  (monster-hit [this x] (generic-monster-hit this x))
  )

;; ---[ Hydra ]--- ;;
(defrecord+defaults Hydra [health (randval 10)]
  Monster
  (do-monster-show
   [this]
   (println (format "A malicious hydra with %d heads" (:health this))))

  (monster-attack
   [this player]
   (let [x (randval (bit-shift-right (:health this) 1))]
     (print "A hydra attacks you with" x "of its heads! ")
     (println "It also grows back one more head.")
     (swap! player update-in [:health] - x)
     (->Hydra (inc (:health this)))
     ))

  (monster-hit
   [this x]
   (let [hyd (->Hydra (- (:health this) x))]
     (if (monster-dead? hyd)
       (println "The corpse of the fully decapitated hydra falls to the floor.")
       (println "You lopped" x "of the hydra's heads!")
       )
     hyd))
  )

;; ---[ SlimeMold ]--- ;;

(defrecord+defaults SlimeMold [health (randval 10)
                               sliminess (randval 5)]
  Monster
  (do-monster-show
   [this]
   (println (format "A slime mold with a sliminess of %d" (:sliminess this))))
  
  (monster-attack
   [this player]
   (let [pts (randval (:sliminess this))]
     (println "A slime mold wraps around your legs decreasing your agility by"
              (str pts "!"))
     (swap! player update-in [:agility] - pts)
     (when (zero? (rand-int 2))
       (println "It also squirts in your face, taking away a health point!")
       (swap! player update-in [:health] dec)))
   this
   )

  (monster-hit [this x] (generic-monster-hit this x))
  )

;; ---[ Brigand ]--- ;;
(defrecord+defaults Brigand [health (randval 10)]
  Monster
  (do-monster-show
   [this]
   (println "A brigand appears before you with"
            (:health this) "health points"))

  (monster-attack
   [this player]
   (let [[attr val] (first (sort-by val > @player))]
     (case attr
       :health (do (println "A brigand hits you with his slingshot,"
                            "taking off 2 health points!")
                   (swap! player update-in [:health] - 2))
       :agility (do (println "A brigand catches your leg with his whip,"
                             "taking off 2 agility points")
                    (swap! player update-in [:agility] - 2))
       :strength (do (println "A brigand cuts your arm with his whip,"
                             "taking off 2 strength points")
                    (swap! player update-in [:strength] - 2))
       ))
   this)

  (monster-hit [this x] (generic-monster-hit this x))
  )


(defn monsters-dead? [monsters]
  (every? #(<= (:health @%) 0) monsters))

(defn monster-dead? [mon]
  (<= (:health mon) 0))

(defn random-monster [monsters]
  (let [mon (monsters (rand-int (count monsters)))]
    (if (monster-dead? @mon)
      (random-monster monsters)
      mon
      )
    )
  )
