(defproject pluggable "0.1.0-SNAPSHOT"
  :description "Utility library to create plugin architectures"
  ;; :url "http://example.com/FIXME"
  :license {:name "EUPL-1.2 or later"
            :url "https://joinup.ec.europa.eu/collection/eupl/eupl-text-eupl-12"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [metosin/malli "0.20.0"]]
  :repl-options {:init-ns pluggable.core})
