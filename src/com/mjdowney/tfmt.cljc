(ns com.mjdowney.tfmt
  (:require
    [rewrite-clj.node :as n]
    [rewrite-clj.zip :as z]))

;;; rewrite-clj helpers

; try to advance z with f, otherwise return {:end z} - eases backtracking
(defn ?navigate [z f] (let [z' (f z)] (if (z/end? z') {:end z} z')))
(def blank (n/whitespace-node ""))
(defn trimmable? [x] (and (z/whitespace? x) (pos? (z/length x))))
(defn row [zloc] (first (z/position zloc)))
(defn col [zloc] (second (z/position zloc)))

(defn ?triml [zloc r] ; trim whitespace to the left, or nil if no whitespace
  (when-let [whitespace (z/find-next zloc z/left*
                          (every-pred #(= (row %) r) trimmable?))]
    (-> whitespace (z/replace blank) z/right*)))

(defn align [zloc col] ; make the node start at the given col if possible
  (println "aligning:" (some-> zloc z/node))
  (let [[r c] (z/position zloc)]
    (cond
      (= c col) zloc
      (> c col) (if-let [zloc' (?triml zloc r)] (recur zloc' col) zloc)
      :else     (z/insert-space-left zloc (- col c)))))

(defn first-child [zloc pred] ; first direct child satisfying `pred`
  (let [child (-> zloc z/down*)]
    (if-not (pred child)
      (z/find-next zloc z/right* pred)
      child)))

(defn map-children
  "Map `f` over the **non-whitespace** children of `zloc`, where `f` is shaped

      (fn [node first-on-line?] => node')

  and `first-on-line?` is true when the `node` is the first non-whitespace
  child on the line, except for the first line of the form.

  E.g. for the form

    (when-let [foo bar]
      (print foo) (flush)
      foo)

  `first-on-line?` is false for `when-let`, and true for `(print foo)` and the
  final `foo`."
  [f zloc]
  (if-let [child (z/down* zloc)]
    (loop [child child
           r (row child)]
      (if-let [?last-child (:end child)]
        (z/up* ?last-child)
        (if (z/whitespace? child)
          (recur (?navigate child z/right*) r)
          (let [child-row (row child)]
            (if (> child-row r)
              (recur (?navigate (f child true) z/right*) child-row)
              (recur (?navigate (f child false) z/right*) r))))))
    zloc))

;;; Main formatting logic

(def format-recursive? #{:list :uneval :vector :set :map :reader-macro})
(defn alignment [zloc]
  (let [first-child (first-child zloc (complement z/whitespace-or-comment?))]
    (if (and (= (z/tag zloc) :list) (= (z/tag first-child) :token))
      (+ (col zloc) 2)
      (col first-child))))

(defn fmt-inner [zloc]
  (if-not (format-recursive? (z/tag zloc))
    zloc
    (let [child-alignment (alignment zloc)]
      (map-children
        (fn [zloc first-on-line?]
          (cond-> zloc
            first-on-line? (align child-alignment)
            true           fmt-inner))
        zloc))))

(defn fmt [zloc]
  (if-let [end (:end zloc)]
    (z/up end)
    (let [zloc (if-not (z/whitespace? zloc)
                 (-> (align zloc 1) fmt-inner) ; no indent for top-level forms
                 zloc)]
      (recur (?navigate zloc z/right*)))))

;;; Examples / tests

;; TODO: Add linting capabilities, so you can fmt* to get {:zloc _, :errors #{}}
;; TODO: Add a README showing how to run against a file / project
^:rct/test
(comment
  (defn fmts [s] ; Helper function to test formatting a string
    (let [s (-> (z/of-string s {:track-position? true}) fmt z/root-string)]
      (println s)
      (clojure.string/split-lines s)))

  ; Simple nested form
  (fmts
    "                (when something
             (if-let [something-else
                                      #_(foo) bar]
             (println x)
                                             ; where does this go?
   body))")
  ;=>>
  ["(when something"
   "  (if-let [something-else"
   "           #_(foo) bar]"
   "    (println x)"
   "    ; where does this go?"
   "    body))"]

  ; Multi-arity form
  (fmts
    "(defn multi
     ([x]
     (+ x 1))
     ([x y]
      (+ x y)))")
  ;=>>
  ["(defn multi"
   "  ([x]"
   "   (+ x 1))"
   "  ([x y]"
   "   (+ x y)))"]

  ; Form with a reader macro
  (fmts "(do\n(println :foo\n#com.mjdowney.something{:k :v\n:k1 :v1}))")
  ;=>
  ["(do"
   "  (println :foo"
   "    #com.mjdowney.something{:k :v"
   "                            :k1 :v1}))"]
  )
