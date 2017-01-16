(defproject bartleby "0.1.2-SNAPSHOT"
  :description "Faithful (Bib)TeX manipulation"
  :url "https://github.com/chbrown/bartleby"
  :license {:name "MIT"
            :url "https://chbrown.github.io/licenses/MIT/#2016"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/data.json "0.2.6"]
                 [org.clojure/tools.cli "0.3.5"]
                 [the/parsatron "0.0.7"]]
  :plugins [; have to include lein-cloverage here for Travis CI
            [lein-cloverage "1.0.9"]]
  :main bartleby.cli
  :profiles {:uberjar {:aot :all}
             :dev {:source-paths ["dev" "test"]
                   :resource-paths ["test/resources"]
                   :main user}})
