(ns thornydev.evolution.sim
  (:require [clojure.string :as str]))

;; (def ^:dynamic *width* 100)
(def ^:dynamic *width* 82)
(def ^:dynamic *height* 30)
(def ^:dynamic *plant-energy* 80)
(def ^:dynamic *reproduction-energy* 200)
;; rectangle on map where the jungle is
;; first two: x,y of top left corener;
;; last two: width and height
(def ^:dynamic *jungle* [45 10 10 10])

;; ---[ util fns ]--- ;;

(defn half [n]
  (bit-shift-right n 1))

(defn pr+flush [& strs]
  (apply print strs)
  (flush))

(defn prn+flush [& strs]
  (apply println strs)
  (flush))

;; ---[ domain fns ]--- ;;

(defn random-plant
  "chooses a random x,y coordinate within the rectangle
   described by the params.
   @return tuple (vector) of x,y coordinates"
  [left top width height]
  (vector (+ left (rand-int width)) (+ top (rand-int height))))

(defn add-plants
  "Takes the existing plant set and returns a new one
   updated with one new plant in the jungle and one in the steppe."
  [plantset]
  (into plantset
        (vector
         (apply random-plant *jungle*)
         (random-plant 0 0 *width* *height*))))

(defn make-animal []
  {:x (half *width*)
   :y (half *height*)
   :energy 1000
   :dir 0
   :genes (into [] (take 8 (repeatedly #(inc (rand-int 10)))))})

(defn move [animal]
  (let [x (mod (+ (:x animal) *width*
                  (cond
                   (#{1 5} (:dir animal))   0
                   (#{2 3 4} (:dir animal)) 1
                   :else                 -1))
               *width*)

        y (mod (+ (:y animal) *height*
                  (cond
                   (#{7 3} (:dir animal))   0
                   (#{0 1 2} (:dir animal)) 1
                   :else                 -1))
               *height*)

        energy (dec (:energy animal))]
    (into animal [[:x x]
                  [:y y]
                  [:energy energy]])
    ))

(defn turn
  "Turns the aninal by selecting a dir based on the genes of the animal.
   @return a new animal with a newly selected dir (could be the same
           dir as before)"
  [animal]
  (letfn [(angle [genes x]
            (let [xnu (- x (first genes))]
              (if (< xnu 0)
                0
                (inc (angle (rest genes) xnu)))))]
    (let [xrand (rand-int (apply + (:genes animal)))
          newdir {:dir (mod (+ (:dir animal)
                               (angle (:genes animal) xrand))
                            (count (:genes animal)))}
          ]
      (merge animal newdir))))


(defn eat
  "The animal will eat the plant at its location, if one is there.
   @return tuple pair of animal and plant-set after the attempt to eat,
           both modified if the animal ate a plant"
  [animal plants]
  (if-let [plant-pos (plants [(:x animal) (:y animal)])]
    [(assoc-in animal [:energy] (+ (:energy animal) *plant-energy*))
     (disj plants plant-pos)]
    [animal plants]))

(defn reproduce
  [animal]
  (let [e (:energy animal)]
    (if (< e *reproduction-energy*)
      [animal]
      (let [ani1 (update-in animal [:energy] half)
            genes (:genes animal)
            mutation (rand-int (count genes))
            new-gene-val (max 1 (+ (nth genes mutation) (dec (rand-int 3))))
            ani2 (assoc-in ani1 [:genes mutation] new-gene-val)]
        (vector ani1 ani2)
        ))))


(defn eat-cycle
  "Steps through all the animals and lets them attempt to eat
   one at a time.
   @return new list of 'fattened' animals and revised plant
           map with those gone that were eaten"
  [animals plants]
  (loop [orig animals  fattened []  plset plants]
    (if-not (seq orig)
      [fattened plset]
      (let [[a p] (eat (first orig) plset)]
        (recur (rest orig) (conj fattened a) p))
      )
    )
  )

(defn update-world
  "@params
    animals seq of animals
    plants  set of plants (coordinates only)
   @return vector of animals (a seq) and plants (a set)"
  [animals plants]
  (let [animals-1 (filter #(> (:energy %) 0) animals)
        animals-2 (map #(-> % turn move) animals-1)
        [animals-3 plants-1] (eat-cycle animals-2 plants)
        animals-nu (flatten (map reproduce animals-3))]
    (vector animals-nu (add-plants plants-1))))


(defn draw-world
  [animals plants]
  (doseq [y (range *height*)]
    (do
      (println)
      (print "|")
      (doseq [x (range *width*)]
        (do
          (print (cond
                  (some #(and (= x (:x %)) (= y (:y %))) animals) \M
                  (plants [x y]) \*
                  :else \space))
          (print "|")
          )))))

(defn read-int
  "Attempts to read the first integer from a string.
   If an integer is found, that integer is returned as a number.
   If none found, false is returned."
  [input]
  (let [stripped (str/replace input #",|_" "")]
    (if-let [n (re-find #"\d+" stripped)]
      (read-string n)
      false)))

(defn evolve-cycles [n animals plants]
  (loop [anims animals  plset plants  i n]
    (if (= i 0)
      [anims plset]
      (let [[anims-nu plants-nu] (update-world anims plset)]
        (when (zero? (mod i 1000))
          (pr+flush \.))
        (recur anims-nu plants-nu (dec i))
        )
      )
    )
  )

(defn evolution []
  (loop [animals [(make-animal)]  plants (add-plants #{})]
    (draw-world animals plants)
    (pr+flush "\n'quit' or number of rounds: ")

    (let [input (str/trim (read-line))]
      (if (re-matches #":?quit" input)
        (do
          ;; set as global so it can be inpected when leave game loop
          (def anivec animals)
          (prn+flush ":done"))
        (if-let [x (read-int input)]
          (let [[anims-nu plants-nu] (evolve-cycles x animals plants)]
            (recur anims-nu plants-nu))
          (do
            (pr+flush "Input not recognized (number or quit allowed)."
                      "Press enter to continue:")
            (read-line)
            (recur animals plants)))
        )
      )
    )
  )

;; ---[ manual testing helpers ]--- ;;
;; (defn setup []
;;   (def anivec [(make-animal) (make-animal)])
;;   (def plset (add-plants #{}))
;;   (def ppr #'clojure.pprint/pprint))
