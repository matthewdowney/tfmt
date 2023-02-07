(ns com.mjdowney.tfmt.fs
  (:require [clojure.java.io :as io]))

;; File helpers
(defn cpath [^java.io.File f] (.getCanonicalPath f))
(defn path  [^java.io.File f] (.getPath f))
(defn dir?  [^java.io.File f] (.isDirectory f))
(defn ls    [^java.io.File f] (.listFiles f))

(defn pmap*
  "Map the function `f` in parallel over all files under :root that match
  :pred.

  Instead of a supplying a :pred, you can also supply a :regex. If neither are
  present then all .clj(c/s) files are matched.

  :root can be a file, a directory, or a collection of the same."
  [f {:keys [root exclude pred regex] :or {regex #".+\.(clj|cljs|cljc)"}}]
  (let [root (if (coll? root) root [root])

        exclude (into #{} (map (comp cpath io/file)) exclude)
        search-recursive? (fn [f] (and (dir? f) (not (exclude (cpath f)))))

        pred (or pred #(and (not (dir? %)) (re-matches regex (path %))))
        pred (fn [f] (and (pred f) (not (exclude (cpath f)))))]
    (->> root
      (mapcat #(tree-seq search-recursive? ls (io/file %)))
      (filter pred)
      (pmap f))))
