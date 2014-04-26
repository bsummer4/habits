(defproject slideshow "pre-alpha"
  :plugins [[lein-cljsbuild "1.0.2"]]
  :source-paths ["src"]
  :dependencies [
    [org.clojure/clojure "1.5.1"]
    [org.clojure/clojurescript "0.0-2173"]
    [org.clojure/core.async "0.1.267.0-0d7780-alpha"]
    [garden "1.1.5"]
    [cljs-ajax "0.2.3"]
    [sablono "0.2.14"]
    [secretary "1.1.0"]
    [om "0.5.0"]]

  :cljsbuild {
    :builds [{:id "slideshow"
      :source-paths ["src"]
      :compiler {
        :output-to "slideshow.js"
        :output-dir "out"
        :optimizations :none
        :source-map true}}]})
