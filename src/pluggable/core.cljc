(ns pluggable.core
  "Pluggable is a plugin container which supports the creation of plugin
   based architectures in Clojure(Script)."
  (:require [pluggable.container :as c]
            [pluggable.root-plugin :as root]))

(defn load-plugins
  "Load the plugins into the (optional argument, defaulting to '{}')
  initial state 'db', returning the modified state db. This function automatically
  adds the pluggable.root-plugin/plugin to the list of plugins to be loaded in
  the first place. All plugins can, therefore, assume that the root-plugin has been
  loaded."
  [plugins & [db]]
  (c/load-plugins
   (-> [root/plugin]
       (concat plugins)
       vec)
   db))
