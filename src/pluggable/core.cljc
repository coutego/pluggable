(ns pluggable.core
  "Pluggable is a plugin container which supports the creation of plugin
   based architectures in Clojure(Script)."
  (:require [pluggable.container :as c]
            [pluggable.root-plugin :as root]))

(defn- normalize-spec [spec]
  (cond
    (vector? spec) {:constructor spec}
    (map? spec)    spec
    :else          {:constructor spec}))

(defn process-bean-entry
  "Public version of bean entry processing to allow external systems (like plin)
   to re-normalize beans after merging."
  [[k v] capture-source?]
  (let [m (meta v)
        doc (:doc m)
        reagent-component (:reagent-component m)
        spec (normalize-spec v)
        source (when capture-source? (list 'quote v))]
    [k (-> (cond-> spec
             source (assoc :debug/source source)
             doc (assoc :debug/doc doc)
             reagent-component (assoc :debug/reagent-component reagent-component))
           (with-meta m))]))

(defn- process-plugin-map [plugin-map capture-source?]
  (if (and (map? plugin-map) (:beans plugin-map))
    (update plugin-map :beans
            (fn [beans]
              (into {} (map #(process-bean-entry % capture-source?) beans))))
    plugin-map))

(defn- validate-plugin [p]
  (when-not (:id p)
    (throw (ex-info "Plugin must have an :id" {:plugin p})))
  p)

#?(:cljs
   (defn plugin
     "Function that processes a plugin definition.
      Normalizes the structure but does NOT capture source code (introspection).
      
      Returns the plugin map."
     [plugin-map]
     (validate-plugin (process-plugin-map plugin-map false)))
   :default
   (defmacro plugin
     "Macro that processes a plugin definition.
      It captures the source code of bean definitions for introspection purposes,
      normalizes the structure, but performs NO side effects.
      
      Returns the plugin map."
     [plugin-map]
     (let [processed (if (map? plugin-map)
                       (process-plugin-map plugin-map true)
                       plugin-map)]
       `(validate-plugin ~processed))))

(defn load-plugins
  "Load the plugins into the (optional argument, defaulting to '{}')
  initial state 'db', returning the modified state db. This function automatically
  adds the pluggable.root-plugin/plugin to the list of plugins to be loaded in
  the first place. All plugins can, therefore, assume that the root-plugin has been
  loaded."
  [plugins & [db]]
  (when-not (vector? plugins)
    (throw (ex-info "pluggable.core/load-plugins: plugins need to be passed as a vector"
                    {:cause "pluggable.core/load-plugins: plugins need to be passed as a vector"})))
  (c/load-plugins
   (-> [root/plugin]
       (concat plugins)
       vec)
   db))
