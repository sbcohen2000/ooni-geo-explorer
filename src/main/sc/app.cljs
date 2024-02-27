(ns sc.app
  (:require
   [cljs.core.async :refer [chan put! go go-loop <!]]
   [clojure.string :as str]
   [sc.canvas]
   [sc.k1-tree :as k1]
   [sc.map]
   [sc.network]
   [sc.rect]
   [sc.timeline]
   [sc.vector2 :as v]))

(defonce event-stream (chan))

;;;; == Resizing =============================================================

(defonce on-resize
  (fn []
    (put! event-stream [:resize])))

(defn setup-resize-handler
  []
  (.removeEventListener js/window "resize" on-resize)
  (.addEventListener js/window "resize" on-resize)
  (on-resize))

;;;; == Dragging / Clicking ==================================================

;; internal use only, clients should;; only read event-stream
(defonce drag-state (atom nil))

(defonce on-pointer-start
  (fn [ev]
    (when (= 0 (.. ev -button))
      (let [x (.. ev -offsetX)
            y (.. ev -offsetY)]
        (.setPointerCapture (.. ev -currentTarget) (.. ev -pointerId))
        (reset! drag-state {:initial [x y] :started false})))))

(defonce on-pointer-end
  (fn [ev]
      (when @drag-state
        (let [x (.. ev -offsetX)
              y (.. ev -offsetY)]
          ;; If the distance from the initial start point is less
          ;; than the threshold, treat this as a click. Else,
          ;; it's the end of a drag action.
          (if (<= (v/distance (:initial @drag-state) [x y]) 3)
            (put! event-stream [:click {:p [x y]}])
            (put! event-stream [:drag-end @drag-state]))))
      (reset! drag-state nil)))

(defonce on-pointer-move
  (fn [ev]
    (let [x (.. ev -offsetX)
          y (.. ev -offsetY)]
      (if @drag-state
          ;; If the distance from the initial start point is
          ;; greater than some threshold, emit drag events.
          (when (> (v/distance (:initial @drag-state) [x y]) 3)
            ;; If the drag state already contains a point, emit a drag
            ;; event. If we haven't added a point yet, this is the
            ;; first drag event.
            (if (:started @drag-state)
              (put! event-stream [:drag (assoc @drag-state :p [x y])])
              (do
                (swap! drag-state #(assoc % :started true))
                (put! event-stream [:drag-start (assoc @drag-state :p [x y])]))))
          (put! event-stream [:pointer-move {:p [x y]}])))))

(defonce on-touch-start
  (fn [ev] (.preventDefault ev)))

;; https://www.redblobgames.com/making-of/draggable/
(defn setup-pointer-capture-handler
  []
  (let [canvas (.getElementById js/document "canvas")]
    (.removeEventListener canvas "pointerdown"   on-pointer-start)
    (.removeEventListener canvas "pointerup"     on-pointer-end)
    (.removeEventListener canvas "pointercancel" on-pointer-end)
    (.removeEventListener canvas "pointermove"   on-pointer-move)
    (.removeEventListener canvas "touchstart"    on-touch-start)
    (.addEventListener    canvas "pointerdown"   on-pointer-start)
    (.addEventListener    canvas "pointerup"     on-pointer-end)
    (.addEventListener    canvas "pointercancel" on-pointer-end)
    (.addEventListener    canvas "pointermove"   on-pointer-move)
    (.addEventListener    canvas "touchstart"    on-touch-start)))

(defonce on-wheel
  (fn [ev]
    (let [dir (js/Math.sign (.. ev -deltaY))
          x (.. ev -offsetX)
          y (.. ev -offsetY)]
      (put! event-stream [:wheel {:dir dir :p [x y]}]))))

(defn setup-wheel-handler
  []
  (let [canvas (.getElementById js/document "canvas")]
    (.removeEventListener canvas "wheel" on-wheel)
    (.addEventListener canvas "wheel" on-wheel)))

;;;; == Clock ================================================================

(defonce interval-id (atom nil))

(defonce on-clock
  (fn []
    (put! event-stream [:clock])))

(defn setup-clock
  []
  (when @interval-id
    (js/clearInterval @interval-id)
    (reset! interval-id nil))
  (reset! interval-id (js/setInterval on-clock 1000)))

(defonce app-state (atom {}))

(declare handler)
(declare repaint)

(defonce event-loop
  (go-loop []
    (let [ev (<! event-stream)
          state' (handler @app-state ev)]
      (when-not (= state' @app-state)
        (repaint state'))
      (reset! app-state state')
      (recur))))

(def tlh 50)

(defn repaint
  "Repaint the app state."
  [state]
  (let [selected-timestamp (get-in state [:timeline :selected-key])
        selected-datapoint (k1/nearest-value (:data state) selected-timestamp)]
    (sc.canvas/with-offset [0 tlh] (:ctx state)
      (sc.map/paint (:map state) selected-datapoint (:ctx state)))
    (sc.canvas/with-offset [0 0] (:ctx state)
      (sc.timeline/paint (:timeline state) (:ctx state)))))

(defn widget-under-point
  "Return a symbol (either :map or :timeline) indicating the widget
  underneath the given point `p`."
  [state p]
  (cond
    (sc.rect/includes? (:map-rect state) p) :map
    (sc.rect/includes? (:timeline-rect state) p) :timeline
    :else nil))

(defn to-UTC-string
  [ms]
  (let [iso (.toISOString
             (new js/Date (.toUTCString (new js/Date ms))))]
    ;; Cut off the timezone marker, ".000Z", which is meaningless now
    ;; since we're in UTC. Also, the OONI API cannot parse the
    ;; timezone.
    (subs iso 0 (- (count iso) 5))))

(defn to-local-timestamp
  [s]
  (.getTime (new js/Date s)))

(defn handle-clock
  [state]
  (let [map-state      (:map state)
        timeline-state (:timeline state)
        now            (js/Date.now)]
    (if (and (:dirty state)
             (> (- now (:last-interaction state)) 3000))
      (let [ccs               (:visible-ccs map-state)
            [begin-ms end-ms] (:src timeline-state)
            begin-stamp       (to-UTC-string begin-ms)
            end-stamp         (to-UTC-string end-ms)
            query-parameters
            {"since"      begin-stamp
             "until"      end-stamp
             "time_grain" "auto"
             "axis_x"     "measurement_start_day"
             "axis_y"     "probe_cc"
             "test_name"  "web_connectivity"
             ;; "domain"     "wikipedia.org"
             "probe_cc"   (str/join "," (map name ccs))}
            url (str
                 "https://api.ooni.io/api/v1/aggregation"
                 (sc.network/query-string query-parameters))]
        (js/console.log "requesting:" url)
        (go
          (let [res (<! (sc.network/fetch-json url))]
            (put! event-stream [:network-response res])))
        (assoc state :dirty false))
      state)))

(defn handle-network-response
  [state res]
  (let [f (fn [m meas]
            (update m (to-local-timestamp (:measurement_start_day meas))
                    #(conj % meas)))
        by-day (reduce f {} (:result res))
        data' (k1/add-points (:data state) (seq by-day))]
    (js/console.log "by-day:" by-day)
    (js/console.log "tree size:" (k1/n-keys data'))
    (-> state
        (assoc :data data')
        (update :timeline #(sc.timeline/set-data % data')))))

(defn handler
  "Handle events."
  [state [tag props :as ev]]
  (case tag
    :init (let [ctx (sc.canvas/getContext)]
            {:ctx              ctx
             :map              (sc.map/model event-stream)
             :map-rect         [0 tlh (sc.canvas/width ctx) (- (sc.canvas/height ctx) tlh)]
             :timeline         (sc.timeline/model event-stream)
             :timeline-rect    [0 0 (sc.canvas/width ctx) tlh]
             :captured-widget  nil
             :last-interaction (js/Date.now)
             :dirty            true
             :data             (k1/k1-tree)})

    :resize (let [[w' h'] (sc.canvas/resize-canvas (:ctx state))]
              (-> state
                  (assoc  :timeline-rect [0 0 w' tlh])
                  (update :timeline #(sc.timeline/handler % [:resize {:w w' :h tlh}]))
                  (assoc  :map-rect [0 tlh w' (- h' tlh)])
                  (update :map #(sc.map/handler % [:resize {:w w' :h (- h' tlh)}]))))

    :clock (handle-clock state)

    :invalidate (assoc state
                       :last-interaction (js/Date.now)
                       :dirty true)

    :network-response (do
                        (js/console.log "network response:" props)
                        (handle-network-response state props))

    :repaint (let [[tag f] props]
               (case tag
                 :map (update state :map f)
                 :timeline (update state :timeline f)))

    (let [state'
          (case tag
            :drag-start (assoc state :captured-widget (widget-under-point state (:p props)))
            :drag-end   (assoc state :captured-widget nil)
            state)]
      ;; If we have a capture, forward all events to the captured
      ;; widget.
      (if-let [captured (:captured-widget state')]
        (case captured
          :map (update state' :map #(sc.map/handler % ev))
          :timeline (update state' :timeline #(sc.timeline/handler % ev)))
        ;; If we don't have a capture, but the event contains a point,
        ;; forward the event only to the widget under the point.
        (if-let [event-point (:p props)]
          (let [target-widget (widget-under-point state' event-point)]
            (case target-widget
              :map (update state' :map #(sc.map/handler % ev))
              :timeline (update state' :timeline #(sc.timeline/handler % ev))))
          ;; Otherwise, forward the event to all widgets.
          (-> state'
              (update :map      #(sc.map/handler % ev))
              (update :timeline #(sc.timeline/handler % ev))))))))

(defn init!
  []
  (put! event-stream [:init])
  (setup-resize-handler)
  (setup-pointer-capture-handler)
  (setup-wheel-handler)
  (setup-clock)
  (println "init!"))

(defn reload!
  []
  (repaint @app-state)
  (println "reload!"))
