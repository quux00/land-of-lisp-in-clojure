(ns thornydev.wizadv.game
  (:require [clojure.string :as str]))

;; ---[ data structures ]--- ;;

(def nodes
  {:living-room "You are in the living-room. A wizard is snoring loudly on the couch."
   :garden "You are in a beautiful garden. There is a well in front of you."
   :attic "You are in the attic. There is a giant welding torch in the corner."})

(def edges
  {:living-room {:garden [:west :door],
                 :attic  [:upstairs :ladder]}
   :garden      {:living-room [:east :door]}
   :attic       {:living-room [:downstairs :ladder]}})

(def objects [:whiskey :bucket :frog :chain])

;; TODO: this may need to be an atom if the loc can change
(def object-locations (atom
                       {:whiskey :living-room
                        :bucket :living-room
                        :chain :garden
                        :frog :garden}))

(def location (atom :living-room))

;; ---[ pure functions ]--- ;;

(defn describe-location
  "@param
    mnodes map of game nodes
    loc location symbol to look up in mnodes
   @return string description"
  [mnodes loc]
  [(loc mnodes)])

(defn describe-path [edges from to]
  (let [descvec (to (from edges))]
    (str "There is a " (name (second descvec)) " going "
         (name (first descvec)) " from here.")))

(defn describe-all-paths
  "@param
    loc: symbol of the location find all paths out of (eg, :attic)
    edges: map of edges between nodes
   @return list of text phrases describing the paths out"
  [edges loc]
  (map #(describe-path edges loc %) (keys (loc edges))))

(defn objects-at
  "@param
    loc: keyword specifying location (eg, :garden)
    obj-locs map of objects to their locations (all entries are keywords)
   @return list of objects (as keywords) in the location specified"
  [loc obj-locs]
  (map first (loc (group-by second obj-locs))))

(defn describe-objects [loc obj-locs]
  (let [desc #(str "You see a " (name %) " on the floor.")]
    (map desc (objects-at loc obj-locs))))


;; ---[ Imperative Shell ]--- ;;

(defn look []
  (str/join " "
            (concat
             (describe-location @location nodes)
             (describe-all-paths @location edges)
             (describe-objects @location @object-locations))))

(defn fwalk [direction]
  (let [attempt (for [e (@location edges)
                      :when (= direction (get-in e [1 0]))]
                  (first e))]
    (if (empty? attempt)
      "You cannot go that way."
      (do
        (reset! location (first attempt))
        (look)))))

(defmacro walk [direction]
  (fwalk (keyword direction)))


(defn fpickup [object]
  (if (= @location (object @object-locations))
    (do (swap! object-locations assoc object :body)
        (str "You are now carrying the " object))
    "You cannot get that."))

(defmacro pickup [object] (fpickup (keyword object)))


(defn inventory []
  (->> (filter #(= :body (second %)) @object-locations)
       (map #(name (first %)))
       (str/join ", ")
       (apply str "Items: ")))


;; ---[ REPL functions ]--- ;;

(def allowed-commands-set #{'look 'walk 'pickup 'inventory})

(defn game-read []
  (let [cmd (read-string (str "(" (read-line) ")"))]
    (cons (first cmd)
          (map keyword (rest cmd)))))

(defn game-eval [sexp]
  (if (allowed-commands-set (first sexp))
    (eval sexp)
    "I do not know that command."))

(game-eval (game-read))

(defn game-print [txt]
  (doseq [s (map str/trim (re-seq #"[^.?!]+[.?!]?" txt))]
    (println s)))

(defn game-repl []
  (loop [cmd (game-read)]
    (when-not (= :quit (keyword (first cmd)))
      (game-print (game-eval cmd))
      (recur (game-read)))))

