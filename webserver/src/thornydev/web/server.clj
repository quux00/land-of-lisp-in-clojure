(ns thornydev.web.server
  (:require [clojure.string :as str]
            [thornydev.clj-sockets :as sk]
            [server.socket :refer [create-server close-server]]
            [thornydev.web.handlers :as hdlr])
  ;; (:import (java.io BufferedReader InputStreamReader PrintWriter)
  ;;          (java.util.concurrent CyclicBarrier))
  )

(defn rrest [coll]
  (rest (rest coll)))

(defn rrrest [coll]
  (rest (rest (rest coll))))

(defn http-char [c1 c2 & optional]
  (try
    (char (Integer/parseInt (str c1 c2), 16))
    (catch NumberFormatException e
      \space)))

(defn decode-param-recurse [s]-
  (loop [ochars (seq s)  nchars []]
    (if-not (seq ochars)
      (apply str nchars)
      (case (first ochars)
        \% (recur (rrrest ochars)
                  (conj nchars (http-char (first (rest ochars))
                                          (first (rrest ochars)))))
        \+ (recur (rest ochars) (conj nchars \space))
        (recur (rest ochars) (conj nchars (first ochars)))))))

(defn decode-param-re [s]
  (let [frep (fn [str-match]
               (case (first str-match)
                 \% (str (apply http-char (rest (seq str-match))))
                 \+ " "))]
    (str/replace s #"\+|%.." frep)))

(def decode-param decode-param-re)

(defn parse-params [s]
  (let [to-pairs (fn [qstr]
                  (let [nvpairs (str/split qstr #"&|=")]
                    (if (odd? (count nvpairs))
                      (conj nvpairs "")
                      nvpairs)))]
    (->> s to-pairs (apply array-map))))

;; (defn parse-params-pairs [s]
;;   (let [to-pairs (fn [qstr]
;;                   (let [nvpairs (str/split qstr #"&|=")]
;;                     (if (odd? (count nvpairs))
;;                       (conj nvpairs "")
;;                       nvpairs)))]
;;     (->> s to-pairs (partition 2))))


(defn triml-char
  "if +ch+ is the first character of the string +s+
   a new string without that first char will be returned,
   otherwise +s+ is returned"
  [s ch]
  (if (= ch (first s)) (subs s 1) s))

(defn parse-url
  "parses URL (string) into a vector where the first
   entry is a string of the URL minus any query string.
   If the input has a query string, the second entry in the
   vector is a seq of name-value pairs (as a sequence pair)"
  [s]
  (let [[url qstr] (-> s
                       (str/split #"\s+")
                       second
                       (triml-char \/)
                       (str/split #"\?"))]
    (if qstr
      [url (parse-params qstr)]
      [url])))


(defn get-header
  "Reads the HTTP header in from *in*, so that should
   be bound to the socket stream reader
   @return map of key-values from the headers"
  []
  (loop [s (read-line) headers {}]
    (if-not (seq s)
      headers
      (recur (read-line)
             (->> (str/split s #":\s+")
                  (apply array-map)
                  (merge headers))))))


;; TODO: we could write a slurp-some that takes a limit of
;;       how much to read
(defn get-content-params
  "Parses HTTP encoded query params from the *body* of an HTTP request.
   It reads the body from *in*, so make sure that is bound to the
   socket stream reader before calling this fn.
   @params
    header: the header map pulled from HTTP request
   @return map of name-values from query string in the body or
           nil if no body found (or no Content-Length header present)"
  [header]
  (when-let [length (header "Content-Length")]
    (parse-params (slurp *in*))))

;; try with server.socket again: see this: http://angeleah.com/blog/2012/11/27/adding-conditions-to-the-clojure-echo-server.html

(defn close-all [& args]
  (doseq [closeable args] (.close closeable)))

(def keep-running (atom true))

(defn serve [request-handler]
  (with-open [server (sk/socket-server 8080)]
    (while @keep-running
      (with-open [socket  (sk/socket-accept server)
                  sreader (sk/socket-reader socket)
                  swriter (sk/socket-writer socket)]
        (binding [*in* sreader
                  *out* swriter]
          (let [url    (parse-url (read-line))
                path   (first url)
                header (get-header)
                params (merge (first (rest url))
                              (get-content-params header))]
            (if (= "quit" path)
              (reset! keep-running false)
              (request-handler path header params))))))))

;; (def barrier (CyclicBarrier. 2))

;; (defn http-server [in out]
;;   (binding [*in*  (BufferedReader. (InputStreamReader. in))
;;             *out* (PrintWriter. out)]
;;     (let [url    (parse-url (read-line))
;;           path   (first url)
;;           header (get-header)
;;           params (merge (first (rest url))
;;                         (get-content-params header))]
;;       (if (= "quit" path)
;;         (reset! keep-running false)
;;         (hdlr/hello-request-handler path header params))))
;;   (.await barrier))

;; (defn serv2 []
;;   (while @keep-running
;;     (let [socket-server (create-server 8080 http-server)]
;;       (try
;;         (.await barrier)
;;         (finally
;;           (close-server socket-server)
;;           (.reset barrier))))))
