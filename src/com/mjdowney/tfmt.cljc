(ns com.mjdowney.tfmt
  (:require
    [clojure.java.io :as io]
    [rewrite-clj.zip :as z]))

(def blank (rewrite-clj.node/whitespace-node ""))

(defn indent [zloc to-col]
  (println "indenting:" (some-> zloc z/node))
  (let [[row col] (z/position zloc)]
    (cond
      (= col to-col) zloc
      (> col to-col) (let [to-remove (z/left* zloc)]
                       (println "Removing" (some-> to-remove z/node))
                       (println "to the left of" (some-> zloc z/node))
                       (recur (-> to-remove (z/replace blank) #_z/remove* z/right*) to-col))
      :else (z/insert-space-left zloc (- to-col col)))))

(defn ?move [zloc f]
  (or (when-let [zloc' (f zloc)]
        (when-not (z/end? zloc')
          zloc'))
    {::end zloc}))

(defn rightish [zloc] (z/find-next zloc z/right* (complement z/whitespace?)))

(defn fmt [zloc offset]
  (let [zloc (if offset (indent zloc offset) zloc)]
    (println "****")
    (println (z/root-string zloc))
    (println "****")
    (println (z/tag zloc))

    (case (z/tag zloc)
      (:list :vector :set :map)
      (if-not (z/down zloc)
        zloc
        (let [plus-offset (get {:list 2 :map 1 :vector 1 :set 2} (z/tag zloc))]
          (loop [child (z/down zloc)
                 prev-line (first (z/position zloc))]
            (if-let [done (::end child)]
              (z/up done)
              (let [current-line (first (z/position child))]
                (if (> current-line prev-line)
                  (recur
                    (?move (fmt child (+ offset plus-offset)) rightish)
                    current-line)
                  (recur
                    (?move (fmt child (second (z/position child))) rightish)
                    prev-line)))))))
      zloc)))

(comment
  (def zloc
    (z/of-string
      "                (when something
               (if-let [something-else
                                        #_(foo) bar]
               (println x)
     body))"
      {:track-position? true}))

  (println (z/root-string zloc))

  (fmt zloc 1)

  ;; TODO: More precise logic around top level vs nested forms
  ;; TODO: Make sure comments are also indented properly
  ;; TODO: Non-list indentation from first form, e.g. #io.foo.bar{:foo ...
  (loop [zloc (z/of-file (io/resource "example2.clj") {:track-position? true})
         first? true]
    (if-let [done (::end zloc)]
      done
      (recur (-> zloc (fmt (if first? 1 (second (z/position zloc)))) (?move z/right)) false)))

  (fmt
    (z/of-string
      "(if-let [1 2
                    #_3 4]
     )"
      {:track-position? true})
    1)
  )
