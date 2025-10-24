(ns zs.state
  (:require [reagent.core :as r]))

(def default-roi {:x 0.25 :y 0.25 :w 0.5 :h 0.5})

(def min-alpha 1.0)
(def max-alpha 40.0)
(def min-band-low 0.05)
(def max-band-high 3.0)
(def min-band-gap 0.05)

(defonce app-state
  (r/atom {:alpha 20.0
           :band {:low 0.1 :high 0.7}
           :breath-rate 0.0
           :explain? false
           :status :initializing
           :history []
           :session/id nil
           :roi default-roi
           :inspector {:selected-event-id nil
                       :clipboard {:status :idle
                                   :message nil
                                   :ts nil}}
           :timeline {:cursor-index nil
                      :cursor-sample nil
                      :locked? false
                      :last-update-origin :auto}}))

(defn update-alpha! [v]
  (swap! app-state assoc :alpha (-> v (max min-alpha) (min max-alpha))))

(defn update-band! [{:keys [low high]}]
  (swap! app-state
         (fn [state]
           (let [band (:band state)
                 current-low (:low band)
                 current-high (:high band)
                 low (-> (or low current-low)
                         (max min-band-low)
                         (min (- max-band-high min-band-gap)))
                 high (-> (or high current-high)
                          (max (+ low min-band-gap))
                          (min max-band-high))
                 low (min low (- high min-band-gap))]
             (assoc state :band {:low low :high high})))))

(defn toggle-explain! []
  (swap! app-state update :explain? not))

(defn set-breath-rate! [hz]
  (swap! app-state assoc :breath-rate hz))

(defn push-sample! [entry rate-fn]
  (swap! app-state
         (fn [state]
           (let [history' (->> (conj (:history state) entry)
                               (take-last 240)
                               vec)
                 rate (rate-fn history')
                 history (if (seq history')
                           (let [idx (dec (count history'))
                                 sample (nth history' idx)
                                 enriched (assoc sample :breath-rate rate)]
                             (assoc history' idx enriched))
                           history')
                 timeline (:timeline state)
                 locked? (get timeline :locked? false)
                 prev-sample (get timeline :cursor-sample)
                 locate-sample
                 (fn [sample coll]
                   (when (and sample (seq coll))
                     (first (keep-indexed (fn [i itm]
                                            (when (= (:t itm) (:t sample))
                                              i))
                                          coll))))
                 count-history (count history)
                 max-idx (when (pos? count-history) (dec count-history))
                 cursor-index (cond
                                (zero? count-history) nil
                                locked? (or (locate-sample prev-sample history)
                                            (get timeline :cursor-index)
                                            max-idx)
                                :else max-idx)
                 cursor-index (when (and cursor-index max-idx)
                                (-> cursor-index (max 0) (min max-idx)))
                 cursor-sample (when (and cursor-index (seq history))
                                 (nth history cursor-index nil))
                 timeline (-> timeline
                              (assoc :cursor-index cursor-index)
                              (assoc :cursor-sample cursor-sample)
                              (assoc :last-update-origin :auto))]
             (-> state
                 (assoc :history history)
                 (assoc :breath-rate rate)
                 (assoc :timeline timeline))))))

(defn set-status! [status]
  (swap! app-state assoc :status status))

(defn set-roi! [{:keys [x y w h]}]
  (swap! app-state
         (fn [state]
           (let [w (max 0.05 (min 1.0 w))
                 h (max 0.05 (min 1.0 h))
                 x (-> x (max 0.0) (min (- 1.0 w)))
                 y (-> y (max 0.0) (min (- 1.0 h)))]
             (assoc state :roi {:x x :y y :w w :h h})))))

(defn select-event! [event-id]
  (swap! app-state assoc-in [:inspector :selected-event-id] event-id))

(defn clear-selection! []
  (swap! app-state assoc-in [:inspector :selected-event-id] nil))

(defn set-clipboard-status! [status message]
  (swap! app-state assoc-in [:inspector :clipboard]
         {:status status
          :message message
          :ts (.valueOf (js/Date.))}))

(defn reset-clipboard-status! []
  (swap! app-state assoc-in [:inspector :clipboard]
         {:status :idle :message nil :ts nil}))

(defn scrub-timeline! [index]
  (swap! app-state
         (fn [state]
           (let [history (:history state)
                 count-history (count history)
                 max-index (when (pos? count-history) (dec count-history))
                 clamped (when max-index (-> index (max 0) (min max-index)))
                 sample (when (and clamped (seq history))
                          (nth history clamped nil))]
             (if sample
               (-> state
                   (assoc-in [:timeline :cursor-index] clamped)
                   (assoc-in [:timeline :cursor-sample] sample)
                   (assoc-in [:timeline :locked?] true)
                   (assoc-in [:timeline :last-update-origin] :user))
               state)))))

(defn follow-timeline! []
  (swap! app-state
         (fn [state]
           (let [history (:history state)
                 count-history (count history)
                 idx (when (pos? count-history) (dec count-history))
                 sample (when (and idx (seq history))
                          (nth history idx nil))]
             (-> state
                 (assoc-in [:timeline :locked?] false)
                 (assoc-in [:timeline :cursor-index] idx)
                 (assoc-in [:timeline :cursor-sample] sample)
                 (assoc-in [:timeline :last-update-origin] :user))))))

(defn begin-session!
  ([] (begin-session! {}))
  ([{:keys [roi status]}]
   (swap! app-state
          (fn [state]
            (let [roi' (or roi (:roi state) default-roi)
                  status' (or status :initializing)]
              (-> state
                  (assoc :session/id (str (random-uuid)))
                  (assoc :history [])
                  (assoc :breath-rate 0.0)
                  (assoc :status status')
                  (assoc :roi roi')
                  (assoc :inspector {:selected-event-id nil
                                     :clipboard {:status :idle
                                                 :message nil
                                                 :ts nil}})
                  (assoc :timeline {:cursor-index nil
                                    :cursor-sample nil
                                    :locked? false
                                    :last-update-origin :auto})))))))
