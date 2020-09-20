(ns pluggable.core
  (:require [pluggable.container :as c]
            [pluggable.root-plugin :as root]))

(defn load-plugins
  "Load the plugins into the (optional argument, defaulting to '{}')
  initial state 'db', returning the modified state db"
  [plugins & [db]]
  (c/load-plugins
   (-> [root/plugin]
       (concat plugins)
       vec)
   db))
