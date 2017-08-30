(defproject breakform "0.4.0-SNAPSHOT"
  :description "in-REPL debugger-style breakpoints."
  :url "http://example.com/FIXME"
  :license {:name "MIT"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/core.async "0.2.395"]
                 [org.clojure/tools.nrepl "0.2.12"]]
  :repl-options
  {:nrepl-middleware [bf.nrepl/wrap-bf]})


