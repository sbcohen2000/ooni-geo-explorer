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
                (put! event-stream [:drag-start @drag-state]))))))))

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

(defn repaint
  "Repaint the app state."
  [state]
  (sc.canvas/with-offset [0 0] (:ctx state)
    (sc.timeline/paint (:timeline state) (:ctx state)))
  (sc.canvas/with-offset [0 50] (:ctx state)
    (sc.map/paint (:map state) (:ctx state))))

(defn handler
  "Handle events."
  [state [tag & props :as ev]]
  (case tag
    :init {:ctx (sc.canvas/getContext)
           :map (sc.map/model)
           :timeline (sc.timeline/model)}
    :resize (let [[w' h'] (sc.canvas/resize-canvas (:ctx state))]
              (-> state
                  (update :timeline #(sc.timeline/handler % [:resize {:w w' :h 50}]))
                  (update :map #(sc.map/handler % [:resize {:w w' :h (- h' 50)}]))))
    (-> state
        (update :timeline #(sc.timeline/handler % ev))
        (update :map #(sc.map/handler % ev)))))

(defn init!
  []
  (put! event-stream [:init])
  (setup-resize-handler)
  (setup-pointer-capture-handler)
  (setup-wheel-handler)
  (println "init!"))

(defn reload!
  []
  (setup-resize-handler)
  (setup-pointer-capture-handler)
  (setup-wheel-handler)
  (println "reload!"))
