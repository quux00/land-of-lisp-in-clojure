(ns thornydev.evolution.sim)

(def ^:dynamic *width* 100)
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

;; (defn remvec
;;   "Removes an element at index +idx+ from the vector.
;;    Uses subvec to do it efficiently
;;    @return new vector with element removed"
;;   [v idx]
;;   (cond
;;    (not (seq v)) v
;;    (= idx 0) (subvec v 1)
;;    (= (count v) 0) (subvec v 0 (dec (count v)))
;;    :else (into (subvec v 0 idx) (subvec v (inc idx)))))

;; ---[ domain fns ]--- ;;

(defn random-plant
  "chooses a random x,y coordinate within the rectangle
   described by the params.
   @return tuple (vector) of x,y coordinates"
  [left top width height]
  (vector (+ left (rand-int width)) (+ top (rand-int height))))

(defn add-plants [plantset]
  (into plantset
        (vector 
         (apply random-plant *jungle*)
         (random-plant 0 0 *width* *height*)
         )))

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
                  [:y x]
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
    [(assoc-in animal [:energy] (+ (:enegy animal) *plant-energy*))
     (disj plants plant-pos)]
    [animal plants]))

;; (defn reproduce
;;   "@params
;;     idx: index of animal to reproduce in the animals vec"
;;   [animals idx]
;;   (let [animal (animals idx)
;;         e (:energy animal)]
;;     (when (>= e *reproduction-energy*)
;;       (let [ani1 (update-in animal [:energy] half)
;;             genes (:genes animal)
;;             mutation (rand-int (count genes))
;;             new-gene-val (max 1 (+ (nth genes mutation) (dec (rand-int 3))))
;;             ani2 (assoc-in ani1 [:genes mutation] new-gene-val)]
;;         (vector (remvec animals idx) ani1 ani2)
;;         )
;;       )
;;     )
;;   )

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
        )
      )
    )
  )
