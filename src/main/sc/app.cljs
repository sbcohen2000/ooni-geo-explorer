(ns sc.app
  (:require
   [cljs.core.async :refer [chan put! go-loop <!]]
   [sc.canvas]
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

;;;; == Dragging / Clicking ==================================================w

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
      (when @drag-state
        (let [x (.. ev -offsetX)
              y (.. ev -offsetY)]
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
                (put! event-stream [:drag-start (assoc @drag-state :p [x y])]))))))))

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
      (repaint state')
      (reset! app-state state')
      (recur))))

(def tlh 50)

(defn repaint
  "Repaint the app state."
  [state]
  (sc.canvas/with-offset [0 tlh] (:ctx state)
    (sc.map/paint (:map state) (:ctx state)))
  (sc.canvas/with-offset [0 0] (:ctx state)
    (sc.timeline/paint (:timeline state) (:ctx state))))

(defn widget-under-point
  "Return a symbol (either :map or :timeline) indicating the widget
  underneath the given point `p`."
  [state p]
  (cond
    (sc.rect/includes? (:map-rect state) p) :map
    (sc.rect/includes? (:timeline-rect state) p) :timeline
    :else nil))

(defn handle-clock
  [state]
  (let [map      (:map state)
        timeline (:timeline state)
        now      (js/Date.now)]
    (if (and (:dirty state)
             (> (- now (:last-interaction state)) 3000))
      (do
       (println "do request!")
       (assoc state :dirty false))
      state)))

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
             :dirty            true})
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
  (setup-resize-handler)
  (setup-pointer-capture-handler)
  (setup-wheel-handler)
  (setup-clock)
  (println "reload!"))
