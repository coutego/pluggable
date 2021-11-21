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
  (:require [clojure.spec.alpha :as s]))

(s/def ::key keyword?)
(s/def ::handler fn?)
(s/def ::doc string?)
(s/def ::spec (s/or :kw   keyword?
                    :pred fn?))
(s/def ::extension (s/keys :req-un [::key ::handler ::doc]
                           :opt-un [::spec]))
(s/def ::extensions (s/or :nil  nil?
                          :exts (s/coll-of ::extension)))

(defn- process-extension [{:keys [db plugins]}
                          {:keys [key handler spec]}]

  (let [vals (vec (filter #(not (nil? %)) (map key plugins)))]
    (when spec
      (doall (for [val vals]
               (when-not (s/valid? spec val)
                 (throw
                  (ex-info
                   (str "Wrong value for extension " key ": " val) {}))))))
    {:db      (handler db vals)
     :plugins plugins}))

(defn- load-plugin [{:keys [db plugins] :as acc} ;; FIXME: do we need the destructuring?
                    {:keys [extensions]}]
  {:pre [(s/valid? ::extensions extensions)]} ;; FIXME: better error messages

  {:db      (:db (reduce process-extension acc extensions))
   :plugins (rest plugins)})

(defn- loader [db plugins]
  (:db (reduce load-plugin {:db db :plugins plugins} plugins)))

(def ^:no-doc plugin
  {:id     :root-plugin
   :loader loader})
