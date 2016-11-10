(defproject bartleby "0.1.0-SNAPSHOT"
  :description "Faithful (Bib)TeX manipulation"
  :url "https://github.com/chbrown/bartleby"
  :license {:name "MIT", :url "https://chbrown.github.io/licenses/MIT/#2016"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.293"]
                 [org.clojure/data.json "0.2.6"]
                 [the/parsatron "0.0.7"]]
  :plugins [[lein-cljsbuild "1.1.4" :exclusions [[org.clojure/clojure]]]]
)
