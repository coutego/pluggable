(ns pluggable.container
  "The Pluggable plugin container"
  (:require [malli.core :as m]
            [malli.error :as me]
            [clojure.string :as str]))

(def plugin-schema
  [:map
   [:id keyword?]
   [:doc {:optional true} string?]
   [:deps {:optional true} [:sequential keyword?]]
   [:loader {:optional true} fn?]])

(def plugins-schema
  [:sequential plugin-schema])

(defn- crash [msg] (throw (ex-info msg {:cause msg})))
(defn- crash-if [condition msg] (when condition (crash msg)))

(defn- make-plugin-list [plugin parents loaded]
  (cond
    (contains? (set loaded) plugin)
    loaded

    (contains? (set parents) (:id plugin))
    (crash (str "Cyclic dependencies: "
                (:id plugin)
                " depends on itself through "
                (str/join parents ", ")))

    :else
    (let [deps (:deps plugin)
          trans-deps
          (reduce (fn [acc dep]
                    (make-plugin-list dep (conj parents (:id plugin)) acc))
                  loaded
                  (or deps []))]
      (conj trans-deps plugin))))

(defn process-plugin-deps
  "Ensures that dependencies are loaded in the right order, returning the list
   of plugins (which can be longer than the original one, because of declared
   dependencies). It throws an exception if cyclic dependencies are found."
  [plugins]
  (let [super-plugin {:id ::meta-plugin, :deps plugins}
        ret          (butlast (make-plugin-list super-plugin [] []))
        ids          (map :id ret)
        dup          (->> ids frequencies (filter #(> (second %) 1)))]
    (crash-if (> (count dup) 0)
              (str "Duplicate plugin id: " dup))
    ret))

(defn load-plugins-impl [plugins db]
  (if (= (count plugins) 0)
    db
    (recur (rest plugins)
           (if-let [loader (-> plugins first :loader)]
             (loader db plugins)
             db))))

(defn load-plugins
  [plugins & [db]]
  (crash-if (not (vector? plugins))
            "pluggable.core/load-plugins: plugins need to be passed as a vector")
  (crash-if (not (or (nil? db) (map? db)))
            "pluggable.core/load-plugins: db must be a map")
  (when-let [errors (m/explain plugins-schema plugins)]
    (crash-if true
              (str "pluggable.core/load-plugins: plugins does not comply with schema: "
                   (me/humanize errors))))
  (load-plugins-impl (process-plugin-deps plugins) (or db {})))
