(defproject bartleby/bartleby "0.10.1"
  :description "Faithful (Bib)TeX manipulation"
  :url "https://github.com/chbrown/bartleby"
  :license {:name "MIT"
            :url "https://chbrown.github.io/licenses/MIT/#2016-2017"}
  :deploy-repositories [["releases" :clojars]]
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.946"]
                 [org.clojure/data.json "0.2.6"]
                 [org.clojure/data.xml "0.2.0-alpha2"]
                 [org.clojure/tools.cli "0.3.5"]
                 [chbrown/parsatron "0.0.8"]]
  :plugins [[lein-cljsbuild "1.1.7"]]
  :main bartleby.cli
  :cljsbuild {:builds [{:id "test"
                        :source-paths ["src" "test"]
                        :compiler {:output-dir "target/test"
                                   :output-to "target/test/main.js"
                                   :main bartleby.runner
                                   :optimizations :whitespace}}]}
  :profiles {:uberjar {:aot :all}
             :dev {:plugins [[lein-codox "0.10.3"]
                             [lein-cloverage "1.0.10"]
                             [lein-doo "0.1.8"]]
                   :doo {:paths {:rhino "lein run -m org.mozilla.javascript.tools.shell.Main"}}
                   :resource-paths ["test/resources"]
                   :codox {:source-paths ["src"] ; exclude dev/ and test/
                           :source-uri "https://github.com/chbrown/bartleby/blob/v{version}/{filepath}#L{line}"}}
             :repl {:dependencies [[org.clojure/tools.namespace "0.3.0-alpha3"]
                                   [org.clojure/tools.trace "0.7.9"]]
                    :source-paths ["dev"]
                    :repl-options {:init-ns user}}})
