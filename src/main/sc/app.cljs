(ns sc.app
  (:require
   [sc.canvas]
   [cljs.core.async :refer [chan put! go go-loop <! >!]]
   [sc.rect]
   [sc.vector2 :as v]))

(defonce geo-data (atom []))

;;;; == Resizing =============================================================

(defonce on-resize
  (fn []
   (sc.canvas/resize-canvas (sc.canvas/getContext))))

(defn setup-resize-handler
  []
  (.removeEventListener js/window "resize" on-resize)
  (.addEventListener js/window "resize" on-resize)
  (on-resize))

;;;; == Dragging / Clicking ==================================================

(defonce event-stream (chan))
(defonce drag-state (atom nil)) ;; internal use only, clients should
                                ;; only read event-stream

(defonce on-pointer-start
  (fn [ev]
    (when (= 0 (.. ev -button))
      (let [x (.. ev -offsetX)
            y (.. ev -offsetY)]
        (.setPointerCapture (.. ev -currentTarget) (.. ev -pointerId))
        (reset! drag-state {:initial [x y]})))))

(defonce on-pointer-end
  (fn [ev]
      (when @drag-state
        (let [x (.. ev -offsetX)
              y (.. ev -offsetY)]
          ;; If the distance from the initial start point is less
          ;; than the threshold, treat this as a click.
          (when (<= (v/distance (:initial @drag-state) [x y]) 3)
            (put! event-stream [:click {:p [x y]}]))))
      (reset! drag-state nil)))

(defonce on-pointer-move
  (fn [ev]
      (when @drag-state
        (let [x (.. ev -offsetX)
              y (.. ev -offsetY)]
          ;; If the distance from the initial start point is
          ;; greater than some threshold, emit drag events.
          (when (> (v/distance (:initial @drag-state) [x y]) 3)
            (put! event-stream [:drag (assoc @drag-state :p [x y])]))))))

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

(defn fetch-json
  "Request the JSON resource at the given URL, and return a channel
  which will receive a Clojure dictionary representing the JSON
  response."
  [url]
  (let [response (chan)]
    (-> (js/fetch url)
      (.then #(.json %))
      (.then #(js->clj % :keywordize-keys true))
      (.then #(put! response %)))
    response))

(def pi js/Math.PI)
(def tau (* 2 js/Math.PI))

(defn degs-to-rads
  [angle]
  (/ (* pi angle) 180))

(def initial-model
  {:map-viewport [0 0 360 180]})

(defn mercator
  " Mercator projection of the latitude and longitude into pixel coordinates.

  src is a rectangle in degrees, 0 being the north pole, 180, the south pole.
  dst is a rectangle in pixel coordinates.
  "
  [src dst]
  (fn [[lat lon]]
    (let [pi-4 (/ pi 4)
          src-rads (sc.rect/scale src (/ pi 180))
          lat-rads (degs-to-rads lat)
          lon-rads (degs-to-rads lon)
          ;; x is on [-pi, pi]
          x lat-rads
          ;; y is on [-pi, pi]
          y (js/Math.log(js/Math.tan(+ pi-4 (/ lon-rads 2))))

          lat-scale (/ tau (sc.rect/width src-rads))
          lon-scale (/ pi  (sc.rect/height src-rads))
          x-src (* lat-scale (- x (sc.rect/left src-rads)))
          y-src (* lon-scale (- y (sc.rect/top src-rads)))]
      [(* (+ x-src pi) (/ (sc.rect/width dst) tau))
       (* (- pi y-src) (/ (sc.rect/height dst) tau))])))

(defn draw-geometry
  "Draw a country's border geometry from the rectangle src into the destination dst.

  See [[mercator]].
  "
  [geometry src dst ctx]
  (let [proj (mercator src dst)]
    (doseq [polygon geometry]
     (when (seq polygon)
       (let [outer-ring (first polygon)]
         (sc.canvas/polygon outer-ring proj ctx)))
     (doseq [hole polygon]
       (sc.canvas/polygon hole proj ctx)))))

(defn draw-all
  "Draw the entire map"
  []
  (let [ctx (sc.canvas/getContext)
        src [0 0 360 180]
        dst [0 0 500 500]]
    (sc.canvas/clear ctx)
    (doseq [cc (keys @geo-data)]
     (draw-geometry (:geometry (cc @geo-data)) src dst ctx))))

(defn doit
  []
  (go (let [data (<! (fetch-json "./geo-data.json"))]
        (reset! geo-data data))))

(defn init!
  []
  (setup-resize-handler)
  (setup-pointer-capture-handler)
  (doit)
  (println "init!"))

(defn reload!
  []
  (setup-resize-handler)
  (setup-pointer-capture-handler)
  (draw-all)
  (println "reload!"))

(go-loop []
  (let [ev (<! event-stream)]
    (println ev)
    (recur)))
