(ns thornydev.web.handlers)

(defn html [s]
  (format "<html>%s</html>" s))

(defn body [s]
  (format "<body>%s</body>" s))

(defn hello-request-handler [^String path mheader params]
  (if (re-find #"^greeting" path)
    (if (mheader "name")
      (println (html (body (str "Nice to meet you " (mheader "name")))))
      (println (html (body "<form>What is your name? <input name='name' /></form>"))))
    (println (html (body "Sorry don't know that page")))))

(comment
  (hello-request-handler "greeting" {"name" "Larry"} nil)
  )
