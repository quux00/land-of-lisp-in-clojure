(ns thornydev.graph.graphing
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]))

(defn substitute-if [val predfn coll]
  (map #(if (predfn %) val %) coll))

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
                         (when-not (pairs-seen pair)
                           (print (dot-name start))
                           (print "--")
                             (print (dot-name end))
                             (println (str "[label=\"" (dot-label vpath) "\"];")))
                         pair))]
          (recur (rest nodes) (apply conj pairs-seen vpairs)))))))

(defn graph->dot [edges]
  (println "digraph {")
  (edges->dot edges)
  (println "}"))

(defn ugraph->dot [edges]
  (println "graph {")
  (uedges->dot edges)
  (println "}"))

(defn graph-it [m]
  (with-open [w (io/writer (:fname m))]
    (binding [*out* w]
      ((:dotfn m) (:edges m))
      )
    )
  (sh "dot"
      (str "-T" (name (:out-type m)))
      (:fname m)
      "-o" (str "wizout." (name (:out-type m))))
  )

(defn graph->svg [fname edges]
  (let [args {:fname fname
              :edges edges
              :dotfn graph->dot
              :out-type :svg}]
    (graph-it args)))

(defn ugraph->svg [fname edges]
  (let [args {:fname fname
              :edges edges
              :dotfn ugraph->dot
              :out-type :svg}]
    (graph-it args)))

