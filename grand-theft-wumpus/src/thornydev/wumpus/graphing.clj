(ns thornydev.wumpus.graphing
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]))

(defn dot-name [kw]
  (str/upper-case (str/replace (name kw) "-" "_")))

(defn dot-label
  "@param path: either a keyword or a seq/vec of keywords
          that will be put as the label
   @return string with label in parens (in the string),
           such as '(cops)'"
  [path]
  (if (coll? path)
    (let [body (apply str (interpose " " (map name path)))]
      (str "(" body ")"))
    (str "(" (name path) ")")))

(defn nodes->dot [nodes]
  (doseq [n nodes]
    (when (< 1 (count n))
      (let [currpos (some #{"*"} (rest n))]
        (print (first n))
        (print "[label=\"")
        (print (first n))
        (print " - ")
        (print (apply str (interpose " " (map name (rest n)))))
        (if currpos
          (print "\", color = red]")
          (println "\"];"))
        (flush))
      )
    )
  )

(defn uedges->dot [edges]
  (loop [nodes (keys edges) pairs-seen #{}]
    (if (empty? nodes)
      pairs-seen
      (let [start (first nodes)
            vpairs (for [[end info] (edges start)]
                     (let [pair (vec (sort [start end]))]
                       (when-not (pairs-seen pair)
                         (print start)
                         (print "--")
                         (print end)
                         (if info
                           (println (str "[label=\""
                                         (dot-label info) "\"];"))
                           (println ";")
                           ))
                       pair))]
        (recur (rest nodes) (apply conj pairs-seen vpairs))))))

(defn ugraph->dot [edges nodes]
  (println "graph {")
  (nodes->dot nodes)
  (uedges->dot edges)
  (println "}"))

(defn graph-it [m]
  (with-open [w (io/writer (str (:fname m) ".dot"))]
    (binding [*out* w]
      ((:dotfn m) (:edges m) (:nodes m))))
  (sh "dot"
      (str "-T" (name (:out-type m)))
      (str (:fname m) ".dot")
      "-o" (str (:fname m) "." (name (:out-type m))))
  )

(defn ugraph->svg [fname edges nodes]
  (let [args {:fname fname
              :edges edges
              :nodes nodes
              :dotfn ugraph->dot
              :out-type :svg}]
    (graph-it args)))




