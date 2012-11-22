(ns thornydev.wizadv.graphing
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]))

(defn substitute-if [val predfn coll]
  (map #(if (predfn %) val %) coll))

(comment (substitute-if 0 odd? [1 2 3 4 5 6 7 8]))
(comment (substitute-if \e #(Character/isDigit %) "I'm a l33t hack3r!"))

(defn dot-name [kw]
  (str/upper-case (str/replace (name kw) "-" "_")))

(defn dot-label [vpath]
  (let [body (apply str (interpose " " (map name vpath)))]
    (str "(" body ")")))

(defn edges->dot [edges] 
  (doseq [start       (keys edges)
          [end vpath] (start edges)]
    (print (dot-name start))
    (print "->")
    (print (dot-name end))
    (println (str "[label=\"" (dot-label vpath) "\"];"))))

(defn uedges->dot [edges]
  (let [mkpair (fn [e1 e2]
                       (map name (sort [e1 e2])))]
    (loop [nodes (keys edges) pairs-seen #{}]
      (if (empty? nodes)
        pairs-seen
        (let [start (first nodes)
              vpairs (for [[end vpath] (start edges)]
                       (let [pair (mkpair start end)]
                         (do
                           (when-not (pairs-seen pair)
                             (print (dot-name start))
                             (print "--")
                             (print (dot-name end))
                             (println (str "[label=\"" (dot-label vpath) "\"];")))
                           pair)))]
          (recur (rest nodes) (apply conj pairs-seen vpairs))
          ))
      ))
  )

(comment
  (uedges->dot edges))

(defn graph->dot [edges]
  (println "digraph {")
  (edges->dot edges)
  (println "}"))

(defn ugraph->dot [edges]
  (println "graph {")
  (uedges->dot edges)
  (println "}"))

(defn graph->svg [fname edges]
  (with-open [w (io/writer fname)]
    (binding [*out* w]
      (graph->dot edges))
    )
  (sh "dot" "-Tsvg" fname "-o" "wizout.svg"))

(defn ugraph->svg [fname edges]
  (with-open [w (io/writer fname)]
    (binding [*out* w]
      (ugraph->dot edges))
    )
  (sh "dot" "-Tsvg" fname "-o" "uwizout.svg"))

(comment
  (edges->dot edges))
