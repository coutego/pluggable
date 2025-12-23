(ns pluggable.root-plugin
  "This plugin implements some common utilities that all plugins probably would need to use.
   It is loaded by default by Pluggable as the first plugin.

   It defines an :extensions extension point. An extensions key declares
   what extensions a plugin accepts and how to handle them. The :extensions extension point
   must be a collection of maps of the form [key* handler* doc* spec?]. Example:

   {:extensions
     [{:key ::routes
       :handler process-routes
       :doc \"The routes extension <etc.>\"}]
       :spec ::routes-spec}

  A handler must be a function of the form taking as input [db values], where
  db is the accumulated state and values is a vector with all the values associated
  by plugins to the given key."
  (:require [malli.core :as m]
            [malli.error :as me]))

(def extension-schema
  [:map
   [:key keyword?]
   [:handler fn?]
   [:doc string?]
   [:spec {:optional true} [:or keyword? fn?]]])

(def extensions-schema
  [:maybe [:sequential extension-schema]])

(defn- get-extension-value [plugin key strict?]
  (let [contrib (get-in plugin [:contributions key])]
    (if (not (nil? contrib))
      contrib
      (when-not strict?
        (let [root-val (get plugin key)]
          (when (not (nil? root-val))
            ;; :beans is a special key that is expected to be at the root
            (when (not= key :beans)
              (println "WARNING: Extension" key "found at root of plugin" (:id plugin) 
                       "- please move to :contributions"))
            root-val))))))

(defn- process-extension [{:keys [db plugins]}
                          {:keys [key handler spec]}]
  (let [strict? (:pluggable/strict? db)
        vals (vec (filter #(not (nil? %)) (map #(get-extension-value % key strict?) plugins)))]
    (when spec
      (doall (for [val vals]
               (when-let [errors (m/explain spec val)]
                 (throw
                  (ex-info
                   (str "Wrong value for extension " key ": " (me/humanize errors))
                   {:errors errors}))))))
    {:db      (handler db vals)
     :plugins plugins}))

(defn- load-plugin [{:keys [db plugins] :as acc} ;; FIXME: do we need the destructuring?
                    {:keys [extensions]}]
  (when-let [errors (m/explain extensions-schema extensions)]
    (throw (ex-info "Invalid extensions schema" {:errors (me/humanize errors)})))

  {:db      (:db (reduce process-extension acc extensions))
   :plugins (rest plugins)})

(defn- loader [db plugins]
  (:db (reduce load-plugin {:db db :plugins plugins} plugins)))

(def ^:no-doc plugin
  {:id     :root-plugin
   :loader loader})
