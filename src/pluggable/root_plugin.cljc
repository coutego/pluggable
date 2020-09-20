(ns pluggable.root-plugin
  "This plugin implements some common utilities that other plugins probably would need to use.

   In the first place, it defines an :extensions extension point. An extensions key declares
   what extensions a plugin accepts and how to handle them. The :extensions extension point
   must be a collection of maps of the form [key* handler* doc* spec?]. Example:

   {:extensions
     [{:key ::routes
       :handler process-routes
       :doc \"The routes extension <etc.>\"}]
       :spec ::routes-spec} "

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

(defn process-extension [{:keys [db plugins] :as acc}
                         {:keys [key handler doc spec] :as ext}]
  (when spec
    (let [vals (map key plugins)]
      (doall (for [val vals]
               (when-not (s/valid? spec val)
                 (throw
                  (ex-info
                   (str "Wrong value for extension " key ": " val) {})))))))
  {:db (handler db (mapv key plugins))
   :plugins plugins})

(defn load-plugin [{:keys [db plugins] :as acc} ;; FIXME: do we need the destructuring?
                   {:keys [extensions] :as p}]
  {:pre [(s/valid? ::extensions extensions)]} ;; FIXME: better error messages

  {:db      (:db (reduce process-extension acc extensions))
   :plugins (rest plugins)})

(defn loader [db plugins]
  (:db (reduce load-plugin {:db db :plugins plugins} plugins)))

(def plugin
  {:id     :root-plugin
   :loader loader})
