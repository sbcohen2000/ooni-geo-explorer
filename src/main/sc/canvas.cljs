(ns sc.canvas
  (:require [sc.colors]))

(defn getContext []
  (let [canvas (.getElementById js/document "canvas")]
    (.getContext canvas "2d")))

(defn- stroke-style
  [color-sym ctx]
  (set! (.-strokeStyle ctx) (sc.colors/color-code-of-symbol color-sym)))

(defn- fill-style
  [color-sym ctx]
  (set! (.-fillStyle ctx) (sc.colors/color-code-of-symbol color-sym)))

(defn width
  "Get the width of the canvas' drawing area."
  [ctx]
  (.. ctx -canvas -width))

(defn height
  "Get the height of the canvas' drawing area."
  [ctx]
  (.. ctx -canvas -height))

(defn clear
  [ctx]
  (.clearRect ctx 0 0 (width ctx) (height ctx)))

(defn resize-canvas
  "Set the canvas to fill the window, and set the canvas resolution to
  the actual device resolution."
  [ctx]
  (let [canvas (.. ctx -canvas)
        dpr (or js/devicePixelRatio 1)
        css-px-width (.-innerWidth js/window)
        device-px-width (* dpr css-px-width)
        css-px-height (.-innerHeight js/window)
        device-px-height (* dpr css-px-height)]
    (set! (.. canvas -style -width) (str css-px-width "px"))
    (set! (.. canvas -style -height) (str css-px-height "px"))
    (set! (.. canvas -width) device-px-width)
    (set! (.. canvas -height) device-px-height)
    (.scale ctx dpr dpr)))

(defn degs-to-rads
  [angle]
  (/ (* js/Math.PI angle) 180))

(def pi js/Math.PI)
(def tau (* 2 js/Math.PI))

(defn mercator
  [lat lon w h phase]
  (let [pi-4 (/ pi 4)
        lat-rads (degs-to-rads lat)
        lon-rads (degs-to-rads lon)
        x (- lat-rads phase)
        y (js/Math.log(js/Math.tan(+ pi-4 (/ lon-rads 2))))]
    [(* (+ x pi) (/ w tau)) (* (- pi y) (/ h tau))]))

(defn polygon
  [p w h ctx]
  (when (seq p)
    (let [[lat lon] (first p)
          [x' y'] (mercator lat lon w h 0)]
      (.beginPath ctx)
      (.moveTo ctx x' y')))
  (doseq [[lat lon] p]
    (let [[x' y'] (mercator lat lon w h 0)]
      (.lineTo ctx x' y')))
  (.stroke ctx))

(defn draw-picture
  [picture ctx])
