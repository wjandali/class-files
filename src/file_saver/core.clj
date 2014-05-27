(ns file_save.core
  (:require [clojure.java.io :as io])
  (:import (java.net URL))
  (:import (java.io File))
  (:import (java.io BufferedReader InputStreamReader)))

(def links-reg #"<a[^>]+>")

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

(defn link-tree [url, height]
  (if (= 0 height)
    (list url)
    (conj  (map #(link-tree % (dec height)) (get-pretty-sub-links url)) url)))

(defn rel-path [base-url url]
  (last (re-matches (re-pattern (str (complete-url base-url) "/([^?]+)") ) url)))



(defn compound [a mark]
  (loop [n 1 b [(first a)]]
    (if (= n (count a))
      b
      (recur
       (inc n)
       (conj b (clojure.string/join (re-pattern mark) [(last b) (a n)]))))))


(defn write-if-file-type [target-dir base-url link file-types]
  (let [path (str target-dir "/" (rel-path base-url link))
        folder-array (compound (clojure.string/split path #"\/" ) "/")]
    (if (and path (contains? file-types (last (re-matches #"[^?]+\.(\w{3,4})" path))) (not (.exists (File. path))))
      (do
        (doall (map #(.mkdir (File. %)) (butlast folder-array)))
        (try (with-open [in (io/input-stream (url link))
                    out (io/output-stream path)]
          (io/copy in out))
        (catch Exception e -1))))))


(defn write-or-create [target-dir tree base-url file-types]
  (if (empty? (rest tree))
    nil
    (do
      (doall (map #(write-if-file-type target-dir base-url % file-types) (filter #(re-matches #"[^?]+\.(\w{3,4})" %) (direct-children tree))))
      (doall (map #(write-or-create target-dir % base-url file-types) (filter #(not (re-matches #"[^?]+\.(\w{3,4})" (first %))) (rest tree)))))))


(defn direct-children [tree]
  (map #( first %) (rest tree)))

(defn subtrees [tree]
  (rest tree))

(def my-url "http://www.cs.berkeley.edu/~jrs/61bf06/")


(write-or-create "/home/wadud/target" tree (first tree) #{"pdf" "txt" "java"})
