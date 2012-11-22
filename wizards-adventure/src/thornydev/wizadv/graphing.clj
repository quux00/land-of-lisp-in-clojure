(ns thornydev.wizadv.graphing
  (:require [clojure.string :as str]))

(defn substitute-if [val predfn coll]
  (map #(if (predfn %) val %) coll))

(comment (substitute-if 0 odd? [1 2 3 4 5 6 7 8]))
(comment (substitute-if \e #(Character/isDigit %) "I'm a l33t hack3r!"))

;; LIVING_ROOM->GARDEN[label="(WEST DOOR)"];
(defn dot-name [kw]
  (str/upper-case (str/replace (name kw) "-" "_")))

(defn dot-label [vpath]
  (let [body (apply str (interpose " " (map name vpath)))]
    (str "(" body ")")))

(defn edges->dot [edges] 
  (doseq [start (keys edges)]
    (doseq [[end vpath] (start edges)]
      (do
        (print (dot-name start))
        (print "->")
        (print (dot-name end))
        (println (str "[label=\"" (dot-label vpath) "\"];")))
      )))

(comment
  (edges->dot edges))
