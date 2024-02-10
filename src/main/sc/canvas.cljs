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

(defn draw-picture
  [picture ctx])
