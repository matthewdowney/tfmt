Babashka script to lint / format code according to [Tonsky formatting](https://tonsky.me/blog/clojurefmt/).

Just something I wanted to try out; prefer [cljfmt](https://github.com/weavejester/cljfmt) 
for serious use.

## Usage

Linting an entire directory:
```
$ bb -x com.mjdowney.tfmt/lint --root "src"
src/com/mjdowney/tfmt.clj:154-157: bad indentation
```

Linting from stdin
```
$ cat example.edn | bb -x com.mjdowney.tfmt/lint --stdin
stdin:2-4: bad indentation
stdin:6-10: bad indentation
```

Formatting from stdin:
```
$ cat example.edn
  (when something
             (if-let [something-else
                                      #_(foo) bar]
             (println x)

                                             ; where does this go?
   (println 
:foo
           #com.mjdowney.something{:k :v
                             :k1 :v1})))

$ cat example.edn |  bb -x com.mjdowney.tfmt/fmt --stdin
(when something
  (if-let [something-else
           #_(foo) bar]
    (println x)

    ; where does this go?
    (println 
      :foo
      #com.mjdowney.something{:k :v
                              :k1 :v1})))
```

