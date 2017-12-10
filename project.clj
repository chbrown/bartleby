(defproject bartleby/bartleby "0.10.1"
  :description "Faithful (Bib)TeX manipulation"
  :url "https://github.com/chbrown/bartleby"
  :license {:name "MIT"
            :url "https://chbrown.github.io/licenses/MIT/#2016-2017"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/data.json "0.2.6"]
                 [org.clojure/data.xml "0.2.0-alpha2"]
                 [org.clojure/tools.cli "0.3.5"]
                 [chbrown/parsatron "0.0.8"]]
  :main bartleby.cli
  :deploy-repositories [["releases" :clojars]]
  :profiles {:uberjar {:aot :all}
             :dev {:plugins [[lein-codox "0.10.3"]]
                   :source-paths ["dev" "test"]
                   :resource-paths ["test/resources"]
                   :codox {:source-paths ["src"] ; exclude dev/ and test/
                           :source-uri "https://github.com/chbrown/bartleby/blob/v{version}/{filepath}#L{line}"}
                   :dependencies [[org.clojure/tools.namespace "0.3.0-alpha3"]
                                  [org.clojure/tools.trace "0.7.9"]]
                   :repl-options {:init-ns user}}
             :test {:plugins [[lein-cloverage "1.0.10"]]
                    :source-paths ["test"]
                    :resource-paths ["test/resources"]}})
