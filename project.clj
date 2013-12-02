(defproject tree "0.0.1-SNAPSHOT"
      :description "FIXME: write description"
      :dependencies [[org.clojure/clojure "1.5.1"]
                     [net.drib/strokes "0.4.1"]
                     ]
      :min-lein-version "2.0.0"
      :source-paths ["src/clj" "src/cljs"]

      :plugins [[lein-cljsbuild "0.3.0"]]

      :cljsbuild {:builds [{:source-paths ["src/cljs"]
                            :compiler {:output-to "public/out/tree.js"
                                       :externs ["../strokes/public/d3/d3-externs.js"]
                                       :pretty-print true
                                    ;   :optimizations :advanced}}]})
                                       :optimizations :simple}}]})
