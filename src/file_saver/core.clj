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
  (try (apply str (line-seq (BufferedReader. (InputStreamReader. (.openStream url)))))
    (catch Exception e "")))



(defn list-tags [r url]
  (re-seq r (url-to-html url)))

(defn get-attrs [attr list]
  (let [ r (re-pattern (str attr "=\"([^\"]+)\""))]
    (filter #(not (nil? %)) (map #(last (re-find r %)) list))))


(defn chopped-addresses [list]
  (filter #(not (nil? %)) (map #(last (re-find #"(http[^?]+)?" %)) list)))

(defn get-pretty-links [url]
  (set (chopped-addresses (get-attrs "href" (list-tags links-reg url)))))

(get-pretty-links "http://www.cs.berkeley.edu/~jrs/61bf06/")

(defn download-image-to-cd [link]
  (let [site-url (url link)]
    (with-open [in (io/input-stream site-url)
              out (io/output-stream "src/test.jpg")]
      (io/copy in out))))

(download-image-to-cd "http://upload.wikimedia.org/wikipedia/commons/1/17/Tiger_in_Ranthambhore.jpg")

(System/getProperty "user.dir")



;; main: url
;; if url is in db ignore it
;; append url to to-visit (ordered array)
;; initiate two threads
;; thread 1: takes last element in to-visit and downloads all images into the current dir
;; thread 2:

;;;;;;;
(def my-url "http://www.cs.berkeley.edu/~jrs/61bf06/")


(defn complete-url [url]
  (if (= \/ (last url))
    (apply str (butlast url))
    url))

(defn rel-to-abs [base, rel]
  (if (= \/ (first rel))
         (str base rel)
         (clojure.string/join #"/"
                       (apply conj (vec (drop-last (dec (count (clojure.string/split rel #"\.\./"))) (clojure.string/split base #"\/")))
                              (filter #(not (empty? %)) (clojure.string/split rel #"\.\.\/"))))))

(defn sub-addresses [list, base-url]
  (concat
   (filter #(re-find (re-pattern (apply str "^" (complete-url base-url) "/[^?]+")) %) list)
   (map #(rel-to-abs (complete-url base-url) %) (filter #(re-find #"^[^:][^:]+[^:]$"  %) list))))

(defn get-pretty-sub-links [url]
  (set (sub-addresses (get-attrs "href" (list-tags links-reg url)) url)))

(get-pretty-sub-links my-url)

(defn link-tree [url, height]
  (if (= 0 height)
    (list url)
    (conj  (map #(link-tree % (dec height)) (get-pretty-sub-links url)) url)))

(def tree (link-tree my-url 2))

(first (rest tree))

(first (rest tree))

(get-attrs "href" (list-tags links-reg my-url))

(defn sub-links-set [url, height]
  (if (= height 0)
    '(url)
    (let [sub-links (get-sub-links url)]
    (conj '(url) (map (fn [u] (sub-links-set u (dec height))) sub-links)))))  ;;should return a tree
