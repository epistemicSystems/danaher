(ns zs.store
  (:require [cljs.pprint :as pp]
            [cljs.reader :as reader]
            [reagent.core :as r]))

(def storage-key "zenith-strider/events")
(def max-events 2000)

(defonce events* (r/atom []))

(defn- storage []
  (try
    (.-localStorage js/window)
    (catch :default _ nil)))

(defn load! []
  (when-let [store (storage)]
    (when-let [raw (.getItem store storage-key)]
      (try
        (let [data (reader/read-string raw)]
          (when (vector? data)
            (reset! events* data)))
        (catch :default err
          (js/console.warn "Failed to parse stored events" err)
          (.removeItem store storage-key))))))

(defn- persist! []
  (when-let [store (storage)]
    (try
      (.setItem store storage-key (pr-str @events*))
      (catch :default err
        (js/console.warn "Failed to persist events" err)))))

(defn record!
  "Append an event map with mandatory :event/type and :session/id keys.
   Automatically enriches with :event/id and :ts before persisting."
  [event]
  (let [event-type (:event/type event)
        session-id (:session/id event)]
    (when (and event-type session-id)
      (let [now (js/Date.)
            enriched (-> event
                         (assoc :event/id (str (random-uuid)))
                         (assoc :ts (.toISOString now)))]
        (swap! events*
               (fn [events]
                 (->> (conj events enriched)
                      (take-last max-events)
                      vec)))
        (persist!)
        enriched))))

(defn recent-events [n]
  (take-last n @events*))

(defn event-by-id [event-id]
  (when event-id
    (some #(when (= (:event/id %) event-id) %) @events*)))

(defn session-events [session-id]
  (when session-id
    (filterv #(= (:session/id %) session-id) @events*)))

(defn export-session-edn [session-id]
  (when-let [events (not-empty (session-events session-id))]
    (binding [*print-namespace-maps* false]
      (with-out-str (pp/pprint events)))))

(defn clear! []
  (reset! events* [])
  (persist!))
