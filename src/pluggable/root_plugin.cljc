(ns pluggable.root-plugin
  "The Root Plugin is automatically loaded by Pluggable as the very first plugin.
   It provides foundational extension points that all other plugins can use.

   ## Core Responsibilities

   1. **Extension Processing**: Implements the `:extensions` mechanism that allows
      plugins to declare extension points and handlers.

   ## Extension Points Provided

   ### `:extensions`
   Allows plugins to declare their own extension points. Each extension definition
   is a map with the following keys:

   | Key       | Required | Description |
   |-----------|----------|-------------|
   | `:key`    | Yes      | The keyword that other plugins will use to contribute |
   | `:handler`| Yes      | A function `(fn [db vals] ...)` that processes contributions |
   | `:doc`    | Yes      | Documentation string describing the extension |"
  (:require [malli.core :as m]
            [malli.error :as me]))

(def extension-schema
  [:map
   [:key keyword?]
   [:handler fn?]
   [:doc string?]
   [:spec {:optional true} [:or keyword? fn?]]])

(def extensions-schema
  "Schema for a collection of extension definitions.

   Plugins declare extensions as a vector of extension maps."
  [:maybe [:sequential extension-schema]])

;; =============================================================================
;; Extension Processing
;; =============================================================================

(defn- get-extension-value
  "Retrieves the value contributed by a plugin for a given extension key.

   Looks first in `:contributions`, then falls back to root-level keys
   (with a deprecation warning) unless strict mode is enabled."
  [plugin key strict?]
  (let [contrib (get-in plugin [:contributions key])]
    (if (not (nil? contrib))
      contrib
      (when-not strict?
        (let [root-val (get plugin key)]
          (when (not (nil? root-val))
            (println "WARNING: Extension" key "found at root of plugin" (:id plugin) 
                     "- please move to :contributions")
            root-val))))))

(defn- process-extension
  "Processes a single extension by collecting all plugin contributions and
   invoking the extension's handler.

   Parameters:
   - `acc`: Accumulator map with `:db` and `:plugins`
   - `extension`: The extension definition map

   Returns updated accumulator with handler results merged into `:db`."
  [{:keys [db plugins]}
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

(defn- load-plugin
  "Loads a single plugin by processing all its declared extensions.

   Parameters:
   - `acc`: Accumulator map with `:db` and `:plugins`
   - `plugin`: The plugin being loaded (destructured for `:extensions`)

   Returns updated accumulator."
  [{:keys [plugins] :as acc}
   {:keys [extensions]}]
  (when-let [errors (m/explain extensions-schema extensions)]
    (throw (ex-info "Invalid extensions schema" {:errors (me/humanize errors)})))

  {:db      (:db (reduce process-extension acc extensions))
   :plugins (rest plugins)})

(defn- standard-loader
  "Standard plugin loader that processes all extensions in order."
  [db plugins]
  (let [process-extensions-result
        (reduce load-plugin {:db db :plugins plugins} plugins)]
    (:db process-extensions-result)))

;; =============================================================================
;; Plugin Definition
;; =============================================================================

(def plugin
  "The Root Plugin definition.

   This plugin is automatically prepended to the plugin list by `pluggable.core/load-plugins`.
   It must not be added manually.

   ## Provides

   - `:loader` - The core plugin loading mechanism
   - `:extensions` extension point - For plugins to declare their own hooks"
  {:id     :root-plugin
   :loader standard-loader
   :extensions []})
