(defproject clj-parse "0.4.4-SNAPSHOT"
  :description "A library for parsing Clojure symbols in Clojure."
  :dependencies [[org.clojure/clojure "1.3.0"]]
  :dev-dependencies [[lein-autodoc "0.9.0"]
                     [lein-marginalia "0.6.1"]]
  :aot [clj-parse.core clj-parse.helpers clj-parse.pseudoebnf])
