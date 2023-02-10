(ns com.mjdowney.tfmt
  "Babashka script to lint / format code according to Tonsky formatting[1].

  [1] https://tonsky.me/blog/clojurefmt/"
  {:org.babashka/cli {:coerce {:root [:string] :exclude [:string]}}}
  (:require
    [clojure.java.io :as io]
    [rewrite-clj.node :as n]
    [rewrite-clj.zip :as z]))

;;; rewrite-clj helpers

(def ^:dynamic *indented* "set of reindented line numbers" #{})

(defn trimmable? [x] (and (z/whitespace? x) (pos? (z/length x))))
(defn row [zloc] (first (z/position zloc)))
(defn col [zloc] (second (z/position zloc)))
(def blank (n/whitespace-node ""))

(defn ?navigate
  "Try to advance z with f, otherwise return {:end z} - eases backtracking."
  [z f]
  (let [z' (f z)]
    (if (z/end? z')
      {:end z}
      z')))

(defn ?triml
  "Trim whitespace to the left, or nil if no whitespace."
  [zloc r]
  (when-let [whitespace (z/find-next zloc z/left*
                          (every-pred #(= (row %) r) trimmable?))]
    (-> whitespace (z/replace blank) z/right*)))

(defn align
  "Make the node start at the given col if possible."
  [zloc col]
  (let [[r c] (z/position zloc)]
    (cond
      (= c col) zloc
      (> c col) (if-let [zloc' (?triml zloc r)] (recur zloc' col) zloc)
      :else     (do
                  (set! *indented* (conj *indented* r))
                  (z/insert-space-left zloc (- col c))))))

(defn first-child
  "First direct child satisfying `pred`."
  [zloc pred]
  (let [child (-> zloc z/down*)]
    (if-not (pred child)
      (z/find-next child z/right* pred)
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
      (some-> first-child col))))

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

(defn fmt*
  "Format the given zloc, returning a new zloc and a set of line numbers that
  were reformatted."
  [zloc]
  (binding [*indented* (sorted-set)]
    [(loop [zloc zloc]
       (if-let [end (:end zloc)]
         (z/up end)
         (let [zloc (if-not (z/whitespace? zloc)
                      (-> (align zloc 1) fmt-inner) ; no top-level indentation
                      zloc)]
           (recur (?navigate zloc z/right*)))))
     *indented*]))

;;; File helpers

(defn cpath [^java.io.File f] (.getCanonicalPath f))
(defn path  [^java.io.File f] (.getPath f))
(defn dir?  [^java.io.File f] (.isDirectory f))
(defn ls    [^java.io.File f] (.listFiles f))
(defn file? [f] (instance? java.io.File f))
(defn url?  [f] (instance? java.net.URL f))

(defn pkeep
  "Map the function `f` in parallel over all files under :root, with `keep`
  semantics.

  Options:
    :root - A file, directory, or collection of the same.
    :exclude - A collection of files or directories to exclude from the search.
    :regex - A regex to match files against. Defaults to .+\\.(clj|cljs|cljc).
    :pred - A predicate to match files against. Defaults to the :regex."
  [f {:keys [root exclude pred regex] :or {regex #".+\.(clj|cljs|cljc)"}}]
  (let [root (if (coll? root) root [root])

        exclude (into #{} (map (comp cpath io/file)) exclude)
        search-recursive? (fn [f]
                            (and
                              (not (url? f))
                              (dir? f)
                              (not (exclude (cpath f)))))

        pred (or pred
               #(or
                  (url? %)
                  (and (not (dir? %)) (re-matches regex (path %)))))
        pred (fn [f]
               (and (pred f)
                 (or (url? f) (not (exclude (cpath f))))))]
    (->> root
         (mapcat #(tree-seq search-recursive? ls (if (string? %) (io/file %) %)))
         (filter pred)
         (pmap f)
         (filter some?))))

;;; Main API

(defn -fmt [file-or-url]
  (fmt*
    (if (url? file-or-url)
      (z/of-string (slurp file-or-url) {:track-position? true})
      (z/of-file file-or-url {:track-position? true}))))

(defn lint*
  "Like `lint`, but returns data instead of printing."
  [opts]
  (pkeep
    (fn [f]
      (try
        {:file (if (url? f) (.getPath f) (path f))
         :problems (second (-fmt f))}
        (catch Exception _
          (locking *out* (println "Error linting" (path f)))
          nil)))
    opts))

(defn group-contiguous
  "Partition `nums` into chunks of contiguous runs."
  [nums]
  (reduce
    (fn [xs n]
      (let [chunk (peek xs)
            prv (peek chunk)]
        (if (and prv (= n (inc prv)))
          (assoc xs (dec (count xs)) (conj chunk n))
          (conj xs [n]))))
    []
    nums))

^:rct/test
(comment
  (group-contiguous [1 2 3 5 6 7 9 11 15 17 18 19])
  ;=> [[1 2 3] [5 6 7] [9] [11] [15] [17 18 19]])
  )

(defn lint
  "Lint the files under :root, print warnings for each indentation violation."
  [opts]
  (run!
    (fn [{:keys [file problems]}]
      (doseq [lines (group-contiguous problems)]
        (let [p (if (file? file) (path file) (str ^java.net.URL file))]
          (println
            (str p ":"
              (if (second lines)
                (str (first lines) "-" (peek lines))
                (first lines))
              ":")
            "bad indentation"))))
    (lint* opts)))

(defn fmt
  "Reformat the Clojure source at the given file or URL and print to stdout."
  [file-or-url]
  (-> (-fmt file-or-url) first z/root-string println))

(comment
  ;; E.g. lint this project
  (lint {:root ["src/"]})

  ;; Or something from github
  (def url (io/as-url "https://raw.githubusercontent.com/matthewdowney/rich-comment-tests/main/src/com/mjdowney/rich_comment_tests.clj"))
  (lint {:root url})

  ;; E.g. reformat and print
  (fmt url))

;;; Examples / tests

^:rct/test
(comment
  (require '[clojure.string :as string])
  (defn fmts [s] ; Helper function to test formatting a string
    (let [s (-> s (z/of-string {:track-position? true}) fmt* first z/root-string)]
      (println s "\n")
      (string/split-lines s)))

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

  ; Empty forms
  (fmts " ()
          []
          {}")
  ;=>>
  ["()"
   "[]"
   "{}"]

  ; First element is a comment
  (fmts
    "{; some comment
 :foo :bar
            :baz :qux}")
  ;=>>
  ["{; some comment"
   " :foo :bar"
   " :baz :qux}"]
  )
