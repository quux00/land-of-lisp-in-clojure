(ns thornydev.web.server-test
  (:use clojure.test
        midje.sweet
        thornydev.web.server)
  (:require [clojure.java.io :as jio]))



;; ---[ parse-params ]--- ;;
(def input1 "name1=value1&foo=bar&gender=M&height=11.3")
(def input2 "name1=value1&foo=bar+baz&gender=M&height=")

(deftest test-parse-params
  (testing "all nv pairs present (none blank"
    (let [exp {"name1"  "value1"
               "foo"     "bar"
               "gender"  "M"
               "height"  "11.3"}]
      (is (= exp (parse-params input1))))))

(fact (parse-params input1) => {"name1"  "value1"
                                "foo"     "bar"
                                "gender"  "M"
                                "height"  "11.3"})

(fact (parse-params input2)
      => (and (contains {"height"  ""})
              (contains {"foo"     "bar+baz"})))

(fact (parse-params input2) => {"name1"  "value1"
                                "foo"     "bar+baz"
                                "gender"  "M"
                                "height"  ""})

(fact (parse-params "foo=bar") => {"foo" "bar"})
(fact (parse-params "foo=") => {"foo" ""})
(fact (parse-params "foo=&quux=") => {"foo" "", "quux" ""})

;; (fact (parse-params-pairs input1)
;;       => (and (contains '(("foo" "bar")))
;;               (contains '(("height" "11.3")))
;;               (contains '(("gender" "M")))
;;               (contains '(("name1" "value1")))))

;; (fact (parse-params-pairs input2)
;;       => (and (contains '(("foo" "bar")))
;;               (contains '(("height" "")))
;;               (contains '(("gender" "M")))
;;               (contains '(("name1" "value1")))))

;; (fact (count (parse-params-pairs input2)) => 4)

;; (fact (parse-params-pairs "foo=") => '(("foo" "")))

;; ---[ http-char ]--- ;;
(fact (http-char \3 \F) => \?)
(fact (http-char \Z \F) => \space)  ;; if doesn't parse, returns space

;; ---[ decode-param (2 versions) ]--- ;;
(fact (decode-param-recurse "foo%3F") => "foo?")
(fact (decode-param-re "foo%3F") => "foo?")
(fact (decode-param-recurse "foo+bar") => "foo bar")
(fact (decode-param-re "foo+bar") => "foo bar")


;; ---[ parse-url ]--- ;;
(fact (parse-url "GET /lolcats.html HTTP/1.1") => ["lolcats.html"])
(fact (parse-url "GET /lolcats.html?extra-funny=yes HTTP/1.1")
      => ["lolcats.html" {"extra-funny" "yes"}])
(fact (parse-url "GET /lolcats.html?extra-funny=yes&foo= HTTP/1.1")
      => ["lolcats.html" {"extra-funny" "yes", "foo" ""}])

;; ---[ get-header ]--- ;;

(let [strm (jio/reader (.getBytes "foo: bar\na: b, 123"))]
  (fact (get-header strm) => {"foo" "bar", "a" "b, 123"}))

;; two newlines causes "a: b, 123" to be considered in the body, not header
(let [strm (jio/reader (.getBytes "foo: bar\n\na: b, 123"))]
  (fact (get-header strm) => {"foo" "bar"}))


;; ---[ get-content-params ]--- ;;

(let [strm (jio/reader (.getBytes "Content-Length: 9\n\na=b&abc=123"))]
  (fact (get-content-params (get-header strm) strm) => {"a" "b", "abc" "123"}))


