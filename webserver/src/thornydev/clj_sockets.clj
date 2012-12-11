(ns thornydev.clj-sockets
  (:import (java.io BufferedReader IOException InputStreamReader PrintWriter)
           (java.net ServerSocket Socket InetAddress)))

(defn socket-server [port]
  (println "socket server being created")
  (ServerSocket. port))

(defn socket-accept [^ServerSocket server-socket]
  (println "socket accept")
  (.accept server-socket))

(defn socket-reader [^Socket socket]
    (println "socket-reader called")
  (BufferedReader. (InputStreamReader. (.getInputStream socket))))

(defn socket-writer [^Socket socket]
    (println "socket-writer called")
  (PrintWriter. (.getOutputStream socket) true))
