(defproject bartleby "0.1.1-SNAPSHOT"
  :description "Faithful (Bib)TeX manipulation"
  :url "https://github.com/chbrown/bartleby"
  :license {:name "MIT"
            :url "https://chbrown.github.io/licenses/MIT/#2016"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/data.json "0.2.6"]
                 [the/parsatron "0.0.7"]]
  :main bartleby.cli
  :profiles {:uberjar {:aot :all}
             :dev {:source-paths ["dev" "test"]
                   :resource-paths ["test/resources"]
                   :main user
                   :dependencies [[org.clojure/tools.namespace "0.3.0-alpha3"]]
                   :repl-options {:init (set! *print-length* 50)}}})
