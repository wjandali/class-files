(ns file_save.core
  (:require [clojure.java.io :as io])
  (:import (java.net URL))
  (:import (java.io BufferedReader InputStreamReader)))


; Constants

(def links-reg #"<a[^>]+>")
(def img-reg #"<img[^>]+>")

(defn url [s]
  (URL. s))


(defmulti url-to-html string?)

(defmethod url-to-html true [s]
  (url-to-html (URL. s)))

(defmethod url-to-html false [url]
  (apply str (line-seq (BufferedReader. (InputStreamReader. (.openStream url))))))

(defn list-tags [r url]
  (re-seq r (url-to-html url)))

(defn get-attrs [attr list]
  (let [ r (re-pattern (str attr "=\"([^\"]+)\""))]
    (filter #(not (nil? %)) (map #(last (re-find r %)) list))))


(defn chopped-addresses [list]
  (filter #(not (nil? %)) (map #(last (re-find #"(http[^?]+)?" %)) list)))

(defn get-pretty-links [url]
  (set (chopped-addresses (get-attrs "href" (list-tags links-reg url)))))

(get-pretty-links "http://www.nytimes.com")

(defn download-image-to-cd [link]
  (let [site-url (url link)]
    (with-open [in (io/input-stream site-url)
              out (io/output-stream "test.jpg")]
      (io/copy in out))))

(download-image-to-cd "http://upload.wikimedia.org/wikipedia/commons/1/17/Tiger_in_Ranthambhore.jpg")

(System/getProperty "user.dir")

