(ns file_save.fsaver
  (:require [clojure.java.io :as io])
  (:import (java.net URL))
  (:import (java.io File))
  (:import (java.util.concurrent Executors))
  (:import (java.io BufferedReader InputStreamReader)))


(def *pool* (Executors/newFixedThreadPool
             (+ 2 (.availableProcessors (Runtime/getRuntime)))))

;; things to improve efficiency:
;; 1 -- we read files and search for links, but some are assets. just kill those in the links search?
;; course then we need a different search for our filetypes

;; helpers

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


(defn rel-path [base-url url]
  (last (re-matches (re-pattern (str (complete-url base-url) "/([^?]+)") ) url)))

(defn compound [a mark]
  (loop [n 1 b [(first a)]]
    (if (= n (count a))
      b
      (recur
       (inc n)
       (conj b (clojure.string/join (re-pattern mark) [(last b) (a n)]))))))

;;

(defn url-to-html [url]
  (try (apply str (line-seq (BufferedReader. (InputStreamReader. (.openStream (URL. url))))))
    (catch Exception e "")))

(defn mlinks [url]
  (set (map last (re-seq (re-pattern (apply str "href=\"" "([^\"|^#|^?]+)")) (url-to-html url)))))

(def getlinks (memoize mlinks))

(defn msublinks [url] ;; want to ignore files if possible
  (let [ links (getlinks url)]
    (concat (filter #(re-matches (re-pattern (apply str "^" (complete-url url) "/[^?]+")) %) links)
            (map #(rel-to-abs (complete-url url) %) (filter #(re-find #"^[^:][^:]+[^:]$"  %) links)))))

;;;//
;;;\\\

(defn add-links [url-set]
  (conj url-set (set (reduce clojure.set/union (filter #(not (contains? @got-links %)) (map msublinks (last url-set)))))))


;;
;; a pdf link on a page shouldn't have to be a sublink
;;


(defn sendf [url path]
  (let [path (str path "/" (rel-path (first (first @for-links)) url))
        folder-array (compound (clojure.string/split path #"\/" ) "/")]
    (do
      (println url)
      (doall (map #(.mkdir (File. %)) (butlast folder-array)))
      (try (with-open [in (io/input-stream (URL. url))
                       out (io/output-stream path)]
             (io/copy in out))
        (catch Exception e -1)))))

; doall? V
(defn call-downloader [filetypes path links]
  (dorun (map #(.submit *pool* (fn [] (sendf % path))) (filter #(contains? filetypes (last (re-matches #"[^?]+\.(\w{3,4})" %))) links))))


(defn download-web [depth url filetypes path]
  (dotimes [_ depth]
    (let [links (last @for-links)]
      (do
        (swap! got-links clojure.set/union (last @for-links))
        (swap! for-links add-links)
        (call-downloader filetypes path links)))))


(defn get-assets [url filetypes path & depth]
  (do
    (def for-links (atom [ #{url}]))
    (def got-links (atom {}))
    (def got-assets (atom {}))
    (if (number? depth)
      (download-web depth url filetypes path)
      (download-web 3 url filetypes path))))

(get-assets "http://www.cs.berkeley.edu/~jrs/61bf06/"  #{"pdf" "txt" "java"} "/home/wadud/target")


