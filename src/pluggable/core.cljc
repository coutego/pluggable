(ns pluggable.core
  (:require [pluggable.container :as c]))

(defn load-plugins
  "Load the plugins into the (optional argument, defaulting to '{}')
  initial state 'db', returning the modified state db"
  [plugins & [db]]
  (c/load-plugins plugins db))
