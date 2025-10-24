(ns zs.ui
  (:require [cljs.pprint :as pp]
            [clojure.string :as str]
            [reagent.core :as r]
            [zs.state :as state]
            [zs.store :as store]))

(defn- clamp [v min-v max-v]
  (-> v (max min-v) (min max-v)))

(defn- pointer->roi [^js e start]
  (let [rect (.getBoundingClientRect (.-currentTarget e))
        x0 (.-x start)
        y0 (.-y start)
        x1 (- (.-clientX e) (.-left rect))
        y1 (- (.-clientY e) (.-top rect))
        min-x (clamp (/ (min x0 x1) (.-width rect)) 0.0 1.0)
        min-y (clamp (/ (min y0 y1) (.-height rect)) 0.0 1.0)
        max-x (clamp (/ (max x0 x1) (.-width rect)) 0.0 1.0)
        max-y (clamp (/ (max y0 y1) (.-height rect)) 0.0 1.0)
        w (max 0.05 (- max-x min-x))
        h (max 0.05 (- max-y min-y))]
    {:x (clamp min-x 0.0 (- 1.0 w))
     :y (clamp min-y 0.0 (- 1.0 h))
     :w w
     :h h}))

(defn video-panel [{:keys [roi on-video-ref on-roi-change status]}]
  (r/with-let [dragging? (r/atom nil)]
    (let [{:keys [x y w h]} roi]
      [:div {:class "video-shell"
             :on-pointer-down
             (fn [e]
               (when (= 0 (.-button e))
                 (let [rect (.getBoundingClientRect (.-currentTarget e))
                       start #js {:x (- (.-clientX e) (.-left rect))
                                  :y (- (.-clientY e) (.-top rect))}
                       pointer-id (.-pointerId e)]
                   (.preventDefault e)
                   (reset! dragging? {:id pointer-id :start start})
                   (.setPointerCapture e pointer-id)
                   (on-roi-change (pointer->roi e start)))))
             :on-pointer-move
             (fn [e]
               (when-let [{:keys [start]} @dragging?]
                 (on-roi-change (pointer->roi e start))))
             :on-pointer-up
             (fn [e]
               (when-let [{:keys [id]} @dragging?]
                 (.releasePointerCapture e id)
                 (reset! dragging? nil)))
             :on-pointer-leave
             (fn [e]
               (when-let [{:keys [id]} @dragging?]
                 (.releasePointerCapture e id)
                 (reset! dragging? nil)))
             :on-pointer-cancel
             (fn [e]
               (when-let [{:keys [id]} @dragging?]
                 (.releasePointerCapture e id)
                 (reset! dragging? nil)))}
       [:video {:ref on-video-ref
                :playsInline true
                :muted true
                :autoPlay true}]
       [:div {:class "roi-overlay"
              :style {:left (str (* x 100) "%")
                      :top (str (* y 100) "%")
                      :width (str (* w 100) "%")
                      :height (str (* h 100) "%")}}]
       [:div {:class "roi-hint"}
        (case status
          :running "Drag to focus on your chest"
          :initializing "Waiting for camera"
          :error "WebGPU unavailable"
          "Set capture focus")]])))

(defn- event->edn [event]
  (binding [*print-namespace-maps* false]
    (with-out-str (pp/pprint event))))

(defn- slider [{:keys [label value min max step on-change]}]
  [:label
   [:span label]
   [:input {:type "range"
            :min min
            :max max
            :step step
            :value value
            :on-change #(-> % .-target .-value js/parseFloat on-change)}]
   [:span (-> value (.toFixed 2))]])

(defn- sparkline [history]
  (let [vals (map :amplitude history)
        count (count vals)]
    (if (zero? count)
      [:div {:style {:height "80px" :display "flex" :alignItems "center" :justifyContent "center" :opacity 0.6}}
       "waiting for frames"]
      (let [w 260
            h 80
            max-v (max 0.001 (apply max (map #(js/Math.abs %) vals)))
            points (->> vals
                        (map-indexed
                         (fn [idx v]
                           (let [x (if (= 1 count)
                                     0
                                     (* (/ idx (dec count)) w))
                                 norm (/ v max-v)
                                 y (- (/ h 2) (* norm (/ h 2)))]
                             (str x "," y))))
                        (str/join " "))]
        [:svg {:width w :height h :viewBox (str "0 0 " w " " h)}
         [:polyline {:fill "none"
                     :stroke "#74b9ff"
                     :stroke-width 2
                     :points points}]]))))

(defn- signal-timeline [{:keys [history timeline on-scrub on-follow]}]
  (let [count (count history)
        max-index (if (pos? count) (dec count) 0)
        cursor-index (:cursor-index timeline)
        locked? (:locked? timeline)
        slider-value (if (and (some? cursor-index) (pos? count))
                       (max 0 (min max-index cursor-index))
                       max-index)
        sample (or (:cursor-sample timeline)
                   (last history))
        amplitude (some-> sample :amplitude)
        breath-rate (some-> sample :breath-rate)
        bpm (when (and breath-rate (pos? breath-rate))
              (* breath-rate 60))
        now (js/performance.now)
        age (when-let [t (:t sample)]
              (/ (- now t) 1000.0))
        scrub! (fn [raw]
                 (when (pos? count)
                   (let [idx (js/parseInt raw 10)]
                     (when-not (js/Number.isNaN idx)
                       (on-scrub idx)))))]
    [:div {:class "timeline"}
     [:div {:class "timeline-header"}
      [:h3 "Signal timeline"]
      [:button {:on-click on-follow
                :disabled (or (zero? count) (not locked?))}
       "Follow live"]]
     (if (pos? count)
       [:<>
        [:input {:type "range"
                 :min 0
                 :max max-index
                 :step 1
                 :value slider-value
                 :on-input #(scrub! (.. % -target -value))
                 :on-change #(scrub! (.. % -target -value))}]
        [:div {:class "timeline-meta"}
         [:span {:class "timeline-lock"}
          (if locked?
            "Scrubbed snapshot"
            "Following live")]
         (when amplitude
           [:span (str "amp " (.toFixed amplitude 3))])
         (when bpm
           [:span (str (.toFixed bpm 1) " bpm")])
         (when age
           [:span (str (.toFixed age 2) "s ago")])]]
       [:p {:class "timeline-empty"}
        "Signal history will appear as soon as frames stream in."])]))

(defn- event-kind [k]
  (let [ns-part (namespace k)
        nm-part (str/replace (name k) "-" " ")]
    (str (when ns-part (str ns-part " · ")) nm-part)))

(defn- event-detail [{:keys [event/type payload]}]
  (case event/type
    :signal/sample (let [{:keys [amplitude breath-rate]} payload
                         bpm (when (and breath-rate (pos? breath-rate))
                               (* breath-rate 60))]
                     (str "amp " (.toFixed amplitude 3)
                          (when bpm (str " · bpm " (.toFixed bpm 1)))))
    :ui/roi-adjusted (let [{:keys [roi]} payload]
                       (when roi
                         (str "roi x" (.toFixed (* 100 (:x roi)) 0) "% · y"
                              (.toFixed (* 100 (:y roi)) 0) "% · w"
                              (.toFixed (* 100 (:w roi)) 0) "% · h"
                              (.toFixed (* 100 (:h roi)) 0) "%")))
    :ui/alpha-adjusted (let [{:keys [alpha previous]} payload]
                         (when (number? alpha)
                           (str "α " (.toFixed alpha 2)
                                (when (number? previous)
                                  (str " (was " (.toFixed previous 2) ")")))))
    :ui/band-adjusted (let [{:keys [band previous]} payload
                            {:keys [low high]} band
                            prev-low (:low previous)
                            prev-high (:high previous)]
                        (when (and band (number? low) (number? high))
                          (str (.toFixed low 2) "–" (.toFixed high 2) " Hz"
                               (when (and (number? prev-low) (number? prev-high))
                                 (str " (was " (.toFixed prev-low 2)
                                      "–" (.toFixed prev-high 2) " Hz)")))))
    :ui/timeline-scrubbed (let [{:keys [cursor-index sample]} payload
                                 {:keys [amplitude breath-rate]} sample
                                 bpm (when (and breath-rate (pos? breath-rate))
                                       (* breath-rate 60))]
                             (str "idx " cursor-index
                                  (when amplitude (str " · amp " (.toFixed amplitude 3)))
                                  (when bpm (str " · bpm " (.toFixed bpm 1)))))
    :ui/timeline-follow-live "tracking latest samples"
    :session/start "session initialized"
    :session/stop (let [{:keys [samples breath-rate]} payload
                        samples (or samples 0)
                        bpm (when (and breath-rate (pos? breath-rate))
                              (* breath-rate 60))]
                    (str "session saved · " samples " samples"
                         (when bpm (str " · bpm " (.toFixed bpm 1)))))
    ""))

(defn- copy-text! [text {:keys [pending success failure unavailable empty]}]
  (cond
    (not (seq text))
    (state/set-clipboard-status! :error (or empty "Nothing to copy"))

    (not (.-clipboard js/navigator))
    (state/set-clipboard-status! :error (or unavailable "Clipboard unavailable"))

    :else
    (do
      (state/set-clipboard-status! :pending (or pending "Copying…"))
      (-> (.writeText (.-clipboard js/navigator) text)
          (.then (fn []
                   (state/set-clipboard-status! :success (or success "Copied"))))
          (.catch (fn [err]
                    (js/console.error "Clipboard write failed" err)
                    (state/set-clipboard-status! :error (or failure "Failed to copy"))))))))

(defn- event-log [{:keys [events selected-event-id on-select]}]
  [:div {:class "event-log"}
   [:h3 "Recent events"]
   (if (seq events)
     [:ul
      (for [{:keys [event/id event/type ts] :as ev} events]
        (let [timestamp (try
                           (.toLocaleTimeString (js/Date. ts)
                                                 js/undefined
                                                 #js {:hour "2-digit"
                                                      :minute "2-digit"
                                                      :second "2-digit"})
                           (catch :default _
                             ts))
              selected? (= event/id selected-event-id)
              select! #(on-select event/id)]
          ^{:key event/id}
          [:li {:class (str "event-row" (when selected? " selected"))
                :role "button"
                :tabIndex 0
                :on-click (fn [e]
                            (.preventDefault e)
                            (select!))
                :on-key-down (fn [e]
                               (let [k (.-key e)]
                                 (when (or (= k "Enter") (= k " ") (= k "Space"))
                                   (.preventDefault e)
                                   (select!))))}
           [:div {:class "event-meta"}
            [:span {:class "event-time"} timestamp]
            [:span {:class "event-kind"} (event-kind event/type)]]
           (when-let [detail (not-empty (event-detail ev))]
             [:div {:class "event-detail"} detail])]))]
     [:p {:class "event-empty"}
      "events will appear here once the session starts"])])

(defn- event-inspector [{:keys [selected-event-id clipboard session-id has-events?]}]
  (let [event (store/event-by-id selected-event-id)
        session-edn (store/export-session-edn session-id)]
    (r/use-effect
     (fn []
       (when (and clipboard (not= (:status clipboard) :idle) (:message clipboard))
         (let [timer (js/setTimeout state/reset-clipboard-status! 2400)]
           (fn [] (js/clearTimeout timer)))))
     #js [(:status clipboard) (:ts clipboard)])
    [:div {:class "event-inspector"}
     [:h3 "Event inspector"]
     (cond
       event
       (let [edn (event->edn event)
             detail (not-empty (event-detail event))]
         [:<>
          [:div {:class "event-inspector-meta"}
           [:span {:class "event-inspector-kind"} (event-kind (:event/type event))]
           [:span {:class "event-inspector-time"}
            (or (:ts event) "—")]]
          (when detail
            [:div {:class "event-inspector-detail"} detail])
          [:pre {:class "event-inspector-raw"} edn]
          [:div {:class "event-inspector-actions"}
           [:button {:on-click #(copy-text! edn {:pending "Copying event…"
                                                 :success "Event copied to clipboard"
                                                 :failure "Failed to copy event"})}
            "Copy event EDN"]
           [:button {:on-click #(copy-text! session-edn {:pending "Copying session log…"
                                                         :success "Session log copied"
                                                         :failure "Failed to copy session log"
                                                         :empty "No session events yet"})
                     :disabled (not session-edn)}
            "Copy session log"]]
          (when (and clipboard (not= (:status clipboard) :idle) (:message clipboard))
            [:div {:class (str "clipboard-toast " (name (:status clipboard)))}
             (:message clipboard)])])

       has-events?
       [:p {:class "event-inspector-empty"}
        "Select an event to inspect its payload and copy it as EDN."]

       :else
       [:p {:class "event-inspector-empty"}
        "Events will appear here once the session begins streaming."])]))

(defn controls [{:keys [on-new-session on-clear-events]}]
  (let [on-new-session (or on-new-session (fn [] nil))
        on-clear-events (or on-clear-events (fn [] nil))
        {:keys [alpha band breath-rate explain? status history roi timeline] :as current} @state/app-state
        bpm (* breath-rate 60)
        session-id (:session/id current)
        inspector (:inspector current)
        events (->> (store/recent-events 6) reverse)
        selected-event-id (get inspector :selected-event-id)
        clipboard (get inspector :clipboard)
        samples (count history)
        session-label (when session-id
                        (let [prefix (subs session-id 0 (min 8 (count session-id)))]
                          (str "Session " prefix "…")))]
    [:div {:class "controls"}
     [:div
      [:h2 "Breath Signal"]
      [:p {:style {:margin-top "0.25rem" :opacity 0.7}}
       (case status
         :running "Live signal streaming"
         :initializing "Initializing sensors"
         :error "WebGPU unavailable"
         "Idle")]]
     [:div {:class "session-meta"}
      [:span (or session-label "No session yet")]
      [:span (str samples " samples")]]
     [:div {:class "session-actions"}
      [:button {:on-click #(on-new-session)} "Start new session"]
      [:button {:class "secondary"
                :on-click #(when (js/confirm "Clear all stored events? This cannot be undone.")
                             (on-clear-events))}
       "Clear stored events"]]
     [slider {:label "Gain α"
              :min 1 :max 40 :step 0.5
              :value alpha
              :on-change state/update-alpha!}]
     [slider {:label "Band Low (Hz)"
              :min 0.05 :max 2.0 :step 0.05
              :value (get-in band [:low])
              :on-change #(state/update-band! {:low % :high (:high band)})}]
     [slider {:label "Band High (Hz)"
              :min 0.1 :max 3.0 :step 0.05
              :value (get-in band [:high])
              :on-change #(state/update-band! {:low (:low band) :high %})}]
     [:div
      [:h3 "Breath cadence"]
      [:div {:style {:font-size "2.5rem" :font-weight 700}}
       (if (pos? bpm)
         (str (.toFixed bpm 1) " bpm")
         "--")]]
     [:div
      [:h3 "Capture focus"]
      [:p {:style {:margin-top "0.35rem" :font-size "0.85rem" :opacity 0.7}}
       (str "x=" (.toFixed (* 100 (:x roi)) 0) "% · y=" (.toFixed (* 100 (:y roi)) 0) "% · w="
            (.toFixed (* 100 (:w roi)) 0) "% · h=" (.toFixed (* 100 (:h roi)) 0) "%")]]
     [:div
      [:button {:on-click state/toggle-explain!}
       (if explain? "Hide explanation" "Explain this estimate")]]
     [sparkline history]
     [signal-timeline {:history history
                       :timeline timeline
                       :on-scrub state/scrub-timeline!
                       :on-follow state/follow-timeline!}]
     [event-log {:events events
                 :selected-event-id selected-event-id
                 :on-select state/select-event!}]
     [event-inspector {:selected-event-id selected-event-id
                       :clipboard clipboard
                       :session-id session-id
                       :has-events? (boolean (seq events))}]
     (when explain?
       (let [recent (->> history (take-last 10) (map :amplitude))]
         [:div {:class "explain-panel"}
          [:p "We apply an Eulerian-style temporal band-pass filter over the luminance change of your chest region, amplified by α."]
          [:p (str "Recent amplitudes: " (if (seq recent)
                                           (->> recent (map #(-> % (.toFixed 3))) (str/join ", "))
                                           "n/a"))]
          [:p (str "Detected cadence: " (if (pos? bpm)
                                          (str (.toFixed bpm 1) " bpm")
                                          "insufficient data"))]]))]))
