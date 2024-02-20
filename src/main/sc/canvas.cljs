(ns sc.canvas
  (:require [sc.colors])
  (:require-macros [sc.canvas]))

(defn getContext []
  (let [canvas (.getElementById js/document "canvas")]
    (.getContext canvas "2d")))

(defn- stroke-style
  [color ctx]
  (if (keyword? color)
    (set! (.-strokeStyle ctx) (sc.colors/color-code-of-symbol color))
    (set! (.-strokeStyle ctx) color)))

(defn- fill-style
  [color ctx]
  (if (keyword? color)
    (set! (.-fillStyle ctx) (sc.colors/color-code-of-symbol color))
    (set! (.-fillStyle ctx) color)))

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
  the actual device resolution. Return the size of the canvas in
  pixels."
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
    [device-px-width device-px-height]))

(defn line
  [[x1 y1] [x2 y2] ctx & {:keys [color] :or {color :black}}]
  (.beginPath ctx)
  (.moveTo ctx x1 y1)
  (.lineTo ctx x2 y2)
  (stroke-style color ctx)
  (.stroke ctx))

(defn circle
  [[x y] r ctx & {:keys [color] :or {color :black}}]
  (.beginPath ctx)
  (.arc ctx x y r 0 (* 2 js/Math.PI) false)
  (fill-style color ctx)
  (.fill ctx))

(defn rectangle
  "Draw a rectangle to the canvas."
  [[x y w h] ctx & {:keys [color fill-color] :or {color :black fill-color nil}}]
  (when color
    (stroke-style color ctx)
    (.strokeRect ctx x y w h))
  (when fill-color
    (fill-style fill-color ctx)
    (.fillRect ctx x y w h)))

(defn polygon
  [poly proj ctx & {:keys [color fill-color] :or {color :black fill-color nil}}]
  (.beginPath ctx)
  (when (seq poly)
    (let [p (first poly)
          [x' y'] (proj p)]
      (.moveTo ctx x' y')))
  (doseq [p poly]
    (let [[x' y'] (proj p)]
      (.lineTo ctx x' y')))
  (when color
    (stroke-style color ctx)
    (.stroke ctx))
  (when fill-color
    (fill-style fill-color ctx)
    (.fill ctx)))

(defn text
  "Draw text to the canvas."
  [text [x y] ctx & {:keys [fill-color size] :or {fill-color :black size 14}}]
  (fill-style fill-color ctx)
  (set! (.-font ctx) (str size "px monospace"))
  (set! (.-textBaseline ctx) "top")
  (.fillText ctx text x y))
