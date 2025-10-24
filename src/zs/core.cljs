(ns zs.core
  (:require [promesa.core :as p]
            [reagent.dom :as rdom]
            [zs.state :as state]
            [zs.ui :as ui]
            [zs.gpu :as gpu]
            [zs.store :as store]))

(defonce cleanup* (atom nil))
(defonce video-el* (atom nil))

(defn estimate-breath-rate [history]
  (let [now (js/performance.now)
        window (->> history
                    (filter #(<= (- now (:t %)) 15000))
                    vec)
        peaks (when (>= (count window) 3)
                (let [max-amp (max 0.001 (apply max (map #(js/Math.abs (:amplitude %)) window)))
                      threshold (* 0.35 max-amp)]
                  (->> (partition 3 1 window)
                       (keep (fn [[a b c]]
                               (let [bv (:amplitude b)]
                                 (when (and (> bv (:amplitude a))
                                            (> bv (:amplitude c))
                                            (> (js/Math.abs bv) threshold))
                                   (:t b)))))))]
    (if (and peaks (>= (count peaks) 2))
      (let [intervals (->> (partition 2 1 peaks)
                           (map (fn [[t0 t1]] (/ (- t1 t0) 1000.0))))
            avg-period (when (seq intervals)
                         (/ (reduce + intervals) (count intervals)))]
        (if (and avg-period (pos? avg-period))
          (/ 1.0 avg-period)
          0.0))
      0.0)))

(defn- wait-for-video! []
  (p/create
   (fn [resolve _reject]
     (if-let [video @video-el*]
       (resolve video)
       (let [k (gensym "video")]
         (add-watch video-el* k
                    (fn [_ _ _ new]
                      (when new
                        (remove-watch video-el* k)
                        (resolve new)))))))))

(defn- sample-luminance [video ctx frame-w frame-h {:keys [x y w h]}]
  (when (> (.-readyState video) 1)
    (.drawImage ctx video 0 0 frame-w frame-h)
    (let [image (.getImageData ctx 0 0 frame-w frame-h)
          data (.-data image)
          x0 (js/Math.floor (* x frame-w))
          x1 (js/Math.floor (* (+ x w) frame-w))
          y0 (js/Math.floor (* y frame-h))
          y1 (js/Math.floor (* (+ y h) frame-h))]
      (loop [y y0
             acc 0.0
             cnt 0]
        (if (< y y1)
          (let [row (* y frame-w)
                [acc' cnt']
                (loop [x x0
                       acc acc
                       cnt cnt]
                  (if (< x x1)
                    (let [idx (* (+ row x) 4)
                          r (aget data idx)
                          g (aget data (+ idx 1))
                          b (aget data (+ idx 2))
                          lum (+ (* 0.2126 r) (* 0.7152 g) (* 0.0722 b))]
                      (recur (inc x)
                             (+ acc lum)
                             (inc cnt)))
                    [acc cnt]))]
            (recur (inc y) acc' cnt'))
          (when (pos? cnt)
            (/ (/ acc cnt) 255.0))))))

(defn- start-sample-loop! [video gpu-context]
  (let [canvas (.createElement js/document "canvas")
        w 160
        h 120
        _ (set! (.-width canvas) w)
        _ (set! (.-height canvas) h)
        ctx (.getContext canvas "2d" #js {:willReadFrequently true})
        running? (atom true)
        last-ts (atom nil)
        sample-log-gap 250.0
        last-log-ts (atom (- sample-log-gap))]
    (state/set-status! :running)
    (letfn [(tick [ts]
              (when @running?
                (let [prev (or @last-ts ts)
                      dt (max 0.001 (/ (- ts prev) 1000.0))]
                  (reset! last-ts ts)
                  (when-let [lum (sample-luminance video ctx w h (:roi @state/app-state))]
                    (let [{:keys [alpha band]} @state/app-state
                          params {:alpha alpha
                                  :low (get-in band [:low])
                                  :high (get-in band [:high])
                                  :dt dt}]
                      (-> (gpu/dispatch! gpu-context lum params)
                          (p/then (fn [amp]
                                    (let [new-state (state/push-sample! {:t ts
                                                                         :luminance lum
                                                                         :amplitude amp}
                                                                        estimate-breath-rate)]
                                      (when (>= (- ts @last-log-ts) sample-log-gap)
                                        (reset! last-log-ts ts)
                                        (when-let [recorded (store/record! {:event/type :signal/sample
                                                                            :session/id (:session/id new-state)
                                                                            :payload {:luminance lum
                                                                                      :amplitude amp
                                                                                      :alpha (:alpha new-state)
                                                                                      :band (:band new-state)
                                                                                      :roi (:roi new-state)
                                                                                      :breath-rate (:breath-rate new-state)}})]
                                          (state/select-event! (:event/id recorded)))))))
                          (p/catch (fn [err]
                                     (js/console.error "GPU dispatch failed" err))))))
                  (js/requestAnimationFrame tick)))]
      (js/requestAnimationFrame tick)
      (fn [] (reset! running? false)))))

(defn- setup-camera! [video]
  (p/let [stream (.getUserMedia js/navigator.mediaDevices #js {:video #js {:width 640 :height 480}})]
    (set! (.-srcObject video) stream)
    (.play video)
    (p/let [_ (p/create (fn [resolve _reject]
                          (if (= (.-readyState video) 4)
                            (resolve true)
                            (.addEventListener video "loadeddata" (fn [] (resolve true)) #js {:once true}))))]
      stream)))

(defn- boot! []
  (state/set-status! :initializing)
  (if-not js/navigator.gpu
    (do
      (state/set-status! :error)
      (js/console.error "WebGPU not available"))
    (-> (p/let [video (wait-for-video!)
                stream (setup-camera! video)
                gpu-context (gpu/init-context!)
                cancel-loop (start-sample-loop! video gpu-context)]
          (reset! cleanup*
                  (fn []
                    (cancel-loop)
                    (doseq [track (.getTracks stream)]
                      (.stop track))
                    (state/set-status! :initializing)))
          nil)
        (p/catch (fn [err]
                   (js/console.error "Setup failed" err)
                   (state/set-status! :error))))))

(def video-watch-key ::video-render)
(def roi-event-watch-key ::roi-events)
(def timeline-event-watch-key ::timeline-events)
(def alpha-event-watch-key ::alpha-events)
(def band-event-watch-key ::band-events)

(defn- record-session-start! [session-state]
  (when-let [session-id (:session/id session-state)]
    (store/record! {:event/type :session/start
                    :session/id session-id
                    :payload {:alpha (:alpha session-state)
                              :band (:band session-state)
                              :roi (:roi session-state)}})))

(defn- record-session-stop! [session-state]
  (when-let [session-id (:session/id session-state)]
    (store/record! {:event/type :session/stop
                    :session/id session-id
                    :payload {:breath-rate (:breath-rate session-state)
                              :samples (count (:history session-state))}})))

(defn restart-session! []
  (let [previous @state/app-state
        stop-event (record-session-stop! previous)
        preserved-roi (:roi previous)
        preserved-status (:status previous)
        session-state (state/begin-session! {:roi preserved-roi
                                             :status (or preserved-status :running)})
        start-event (record-session-start! session-state)]
    (when (= preserved-status :running)
      (state/set-status! :running))
    (when stop-event
      (state/select-event! (:event/id stop-event)))
    (when start-event
      (state/select-event! (:event/id start-event)))))

(defn clear-events! []
  (store/clear!)
  (state/clear-selection!)
  (state/reset-clipboard-status!))

(defn- video-ref [el]
  (reset! video-el* el))

(defn- render-video-panel [state]
  [ui/video-panel {:roi (:roi state)
                   :status (:status state)
                   :on-video-ref video-ref
                   :on-roi-change state/set-roi!}])

(defn mount-surfaces []
  (when-let [container (.getElementById js/document "app")]
    (rdom/render [ui/controls {:on-new-session restart-session!
                               :on-clear-events clear-events!}] container))
  (when-let [video-container (.getElementById js/document "video-panel")]
    (rdom/render (render-video-panel @state/app-state) video-container))
  (remove-watch state/app-state video-watch-key)
  (add-watch state/app-state video-watch-key
             (fn [_ _ _ new-state]
               (when-let [video-container (.getElementById js/document "video-panel")]
                 (rdom/render (render-video-panel new-state) video-container)))))
  (remove-watch state/app-state roi-event-watch-key)
  (add-watch state/app-state roi-event-watch-key
             (fn [_ _ old new]
               (when (and (:roi old) (:roi new) (not= (:roi old) (:roi new)))
                 (when-let [recorded (store/record! {:event/type :ui/roi-adjusted
                                                     :session/id (:session/id new)
                                                     :payload {:roi (:roi new)}})]
                   (state/select-event! (:event/id recorded))))))
  (remove-watch state/app-state alpha-event-watch-key)
  (add-watch state/app-state alpha-event-watch-key
             (fn [_ _ old new]
               (let [old-alpha (:alpha old)
                     new-alpha (:alpha new)
                     session-id (:session/id new)]
                 (when (and session-id
                            (number? new-alpha)
                            (not= old-alpha new-alpha))
                   (when-let [recorded (store/record! {:event/type :ui/alpha-adjusted
                                                       :session/id session-id
                                                       :payload {:alpha new-alpha
                                                                 :previous old-alpha}})]
                     (state/select-event! (:event/id recorded)))))))
  (remove-watch state/app-state band-event-watch-key)
  (add-watch state/app-state band-event-watch-key
             (fn [_ _ old new]
               (let [old-band (:band old)
                     new-band (:band new)
                     session-id (:session/id new)]
                 (when (and session-id
                            (map? new-band)
                            (not= old-band new-band))
                   (when-let [recorded (store/record! {:event/type :ui/band-adjusted
                                                       :session/id session-id
                                                       :payload {:band new-band
                                                                 :previous old-band}})]
                     (state/select-event! (:event/id recorded)))))))
  (remove-watch state/app-state timeline-event-watch-key)
  (add-watch state/app-state timeline-event-watch-key
             (fn [_ _ old new]
               (let [old-timeline (:timeline old)
                     new-timeline (:timeline new)
                     origin (:last-update-origin new-timeline)]
                 (when (and new-timeline
                            (not= old-timeline new-timeline)
                            (:session/id new))
                   (let [session-id (:session/id new)
                         new-index (:cursor-index new-timeline)
                         sample (some-> (:cursor-sample new-timeline)
                                        (select-keys [:t :luminance :amplitude :breath-rate]))]
                     (cond
                       (and (not (:locked? old-timeline))
                            (:locked? new-timeline)
                            (some? new-index)
                            (= origin :user))
                       (when-let [recorded (store/record! {:event/type :ui/timeline-scrubbed
                                                           :session/id session-id
                                                           :payload {:cursor-index new-index
                                                                     :sample sample}})]
                         (state/select-event! (:event/id recorded)))

                       (and (:locked? new-timeline)
                            (:locked? old-timeline)
                            (not= (:cursor-index old-timeline) new-index)
                            (some? new-index)
                            (= origin :user))
                       (when-let [recorded (store/record! {:event/type :ui/timeline-scrubbed
                                                           :session/id session-id
                                                           :payload {:cursor-index new-index
                                                                     :sample sample}})]
                         (state/select-event! (:event/id recorded)))

                       (and (:locked? old-timeline)
                            (not (:locked? new-timeline))
                            (= origin :user))
                       (when-let [recorded (store/record! {:event/type :ui/timeline-follow-live
                                                           :session/id session-id
                                                           :payload {:cursor-index new-index
                                                                     :sample sample}})]
                         (state/select-event! (:event/id recorded))))))))))

(defn start []
  (store/load!)
  (let [session-state (state/begin-session!)]
    (when-let [recorded (record-session-start! session-state)]
      (state/select-event! (:event/id recorded))))
  (mount-surfaces)
  (boot!))

(defn stop []
  (when-let [f @cleanup*]
    (f)
    (reset! cleanup* nil))
  (when (:session/id @state/app-state)
    (when-let [recorded (record-session-stop! @state/app-state)]
      (state/select-event! (:event/id recorded))))
  (reset! video-el* nil)
  (remove-watch state/app-state video-watch-key)
  (remove-watch state/app-state roi-event-watch-key)
  (remove-watch state/app-state timeline-event-watch-key)
  (remove-watch state/app-state alpha-event-watch-key)
  (remove-watch state/app-state band-event-watch-key))

(defn init []
  (start))
