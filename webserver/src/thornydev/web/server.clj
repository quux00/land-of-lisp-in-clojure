(ns thornydev.web.server
  (:require [clojure.string :as str]))

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
  [stream]
  (binding [*in* stream]
    (loop [s (read-line) headers {}]
      (if-not (seq s)
        headers
        (recur (read-line) (->> (str/split s #":\s*")
                                (apply array-map)
                                (merge headers)))))))


;; TODO: we could write a slurp-some that takes a limit of
;;       how much to read
(defn get-content-params
  "Parses HTTP encoded query params from the body of an HTTP request.
   @params
    stream: reader from which the header has already been read/pulled
    header: the header map pulled from +stream+
   @return map of name-values from query string in the body or
           nil if no body found (or no Content-Length header present)"
  [stream header]
  (when-let [length (header "Content-Length")]
    (parse-params (slurp stream))))


;; TODO: have to figure out the socket stuff for this
(defn serve [request-handler]
  
  )
