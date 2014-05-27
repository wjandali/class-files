(ns file_save.core
  (:require [clojure.java.io :as io])
  (:import (java.net URL))
  (:import (java.io File))
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

(defn link-tree [url, height]
  (if (= 0 height)
    (list url)
    (conj  (map #(link-tree % (dec height)) (get-pretty-sub-links url)) url)))

(def tree (link-tree my-url 3))

tree

(defn fromto [a,b, tree]
  (take (- b a) (drop a tree)))

(defn rel-path [base-url url]
  (last (re-matches (re-pattern (str (complete-url base-url) "/([^?]+)") ) url)))



(defn compound [a mark]
  (loop [n 1 b [(first a)]]
    (if (= n (count a))
      b
      (recur
       (inc n)
       (conj b (clojure.string/join (re-pattern mark) [(last b) (a n)]))))))


(defn write-if-file-type [base-url link file-types]
  (let [path (rel-path base-url link)
        folder-array (compound (clojure.string/split path #"\/" ) "/")]
    (if (and path (contains? file-types (last (re-matches #"[^?]+\.(\w{3,4})" path))) (not (.exists (File. path))))
      (do
        (doall (map #(.mkdir (File. %)) (butlast folder-array)))
        (with-open [in (io/input-stream (url link))
                    out (io/output-stream path)]
          (io/copy in out))))))



(write-if-file-type "http://www.cs.berkeley.edu/~jrs/61bf06/" "http://www.cs.berkeley.edu/~jrs/61bf06/hw/hw1/readme.pdf" #{"pdf"})
(rel-path "http://www.cs.berkeley.edu/~jrs/61bf06/" "http://www.cs.berkeley.edu/~jrs/61bf06/hw/hw1/readme.pdf")
(compound (clojure.string/split (rel-path "http://www.cs.berkeley.edu/~jrs/61bf06/" "http://www.cs.berkeley.edu/~jrs/61bf06/hw/hw1/readme.pdf") #"\/") "/")

(defn write-or-create [tree base-url file-types]
  (if (empty? (rest tree))
    nil
    (do
      (map #(write-if-file-type base-url % file-types) (filter #(re-matches #"[^?]+\.(\w{3,4})" %) (direct-children tree)))
      (map #(write-or-create % base-url file-types) (filter #(not (re-matches #"[^?]+\.(\w{3,4})" %)) (rest tree))))))



(write-or-create tree (first tree) #{"pdf" "txt" "java"})

(defn direct-children [tree]
  (map #( first %) (rest tree)))

(defn subtrees [tree]
  (rest tree))

tree

(rest (rest tree))
(direct-children tree)

(first (rest tree))

(last tree)

(defn download-image-to-cd [link]
  (let [site-url (url link)]
    (with-open [in (io/input-stream site-url)
              out (io/output-stream "src/test.jpg")]
      (io/copy in out))))

(defn link-tree-to-files [base-path, tree, file-types])
