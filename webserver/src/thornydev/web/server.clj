(ns thornydev.web.server
  (:require [clojure.string :as str]
            [thornydev.clj-sockets :as sk]))

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
  "Reads the HTTP header in from the stream passed in
   @return map of key-values from the headers"
  [sreader]
  (loop [s (.readLine sreader) headers {}]
    (if-not (seq s)
      headers
      (recur (.readLine sreader)
             (->> (str/split s #":\s+")
                  (apply array-map)
                  (merge headers))))))


;; TODO: we could write a slurp-some that takes a limit of
;;       how much to read
(defn get-content-params
  "Parses HTTP encoded query params from the *body* of an HTTP request.
   @params
    header: the header map pulled from +stream+
    sreader: reader from which the header has already been read/pulled
   @return map of name-values from query string in the body or
           nil if no body found (or no Content-Length header present)"
  [header sreader]
  (when-let [length (header "Content-Length")]
    (parse-params (slurp sreader))))

;; try with server.socket again: see this: http://angeleah.com/blog/2012/11/27/adding-conditions-to-the-clojure-echo-server.html

(defn close-all [& args]
  (doseq [closeable args] (.close closeable)))

(defn serv0 [request-handler]
  (with-open [server (sk/socket-server 8080)]
    (loop [count 2]
      (when-not (<= count 0)
        ;; TODO: change the first 3 to with-open
        (let [socket (sk/socket-accept server)
              sreader (sk/socket-reader socket)
              swriter (sk/socket-writer socket)
              url     (parse-url (.readLine sreader))
              path    (first url)
              header  (get-header sreader)
              url-params (first (rest url))
              body-params (get-content-params header sreader)
              params  (merge (first (rest url)) (get-content-params header sreader))]
          ;; (.println swriter
          ;;           (format "<html><body>Hi there: %d<hr>%s<hr>%s<hr>%s</body></html>",
          ;;                   count, path, header, params))
          (binding [*out* swriter]
            (request-handler path header params))
          (close-all swriter sreader socket)
          (recur (dec count))
          )
        )))
  (println "ALL DONE"))

;; TODO: have to figure out the socket stuff for this
;; (defn serve [request-handler]
;;   (with-open [server (sk/socket-server 8080)
;;               socket (sk/socket-accept server)
;;               sreader (sk/socket-reader socket)
;;               swriter (sk/socket-writer socket)]   ;; TODO: could also try bindings *out* and *in* for the rdr/wtr
;;     (println "doing the binding now")
;;     (binding [*in*  sreader
;;               *out* swriter]
;;       (println "about to read-line")
;;       (let 
;;           path (first url)
;;           header (get-header sreader)
;;           params (merge (rest url) (get-content-params header sreader))
;;         (request-handler path header params)))))

;; Lisp version
;; (defun serve (request-handler)
;;   (let ((socket (socket-server 8080)))
;;     (unwind-protect
;;      (loop (with-open-stream (stream (socket-accept socket))
;;              (let* ((url    (parse-url (read-line stream)))
;;                     (path   (car url))
;;                     (header (get-header stream))
;;                     (params (append (cdr url) 
;;                                     (get-content-params stream header)))
;;                     (*standard-output* stream))
;;                    (funcall request-handler path header params))))
;;      (socket-server-close socket))))

