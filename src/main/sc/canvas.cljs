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

(defn- stroke-width
  [width ctx]
  (set! (.-lineWidth ctx) width))

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
  [[x1 y1] [x2 y2] ctx &
   {:keys [color width]
    :or {color :black width 1.0}}]
  (stroke-width width ctx)
  (.beginPath ctx)
  (.moveTo ctx x1 y1)
  (.lineTo ctx x2 y2)
  (stroke-style color ctx)
  (.stroke ctx))

(defn arc
  [[x y] r t0 t1 ctx &
   {:keys [color fill-color width]
    :or {color :black fill-color nil width 1.0}}]
  (stroke-width width ctx)
  (.beginPath ctx)
  (.arc ctx x y r t0 t1 false)
  (.lineTo ctx x y)
  (when color
    (stroke-style color ctx)
    (.stroke ctx))
  (when fill-color
    (fill-style fill-color ctx)
    (.fill ctx)))

(defn rectangle
  "Draw a rectangle to the canvas."
  [[x y w h] ctx &
   {:keys [color fill-color width]
    :or {color :black fill-color nil width 1.0}}]
  (stroke-width width ctx)
  (when color
    (stroke-style color ctx)
    (.strokeRect ctx x y w h))
  (when fill-color
    (fill-style fill-color ctx)
    (.fillRect ctx x y w h)))

(defn polygon
  [poly proj ctx
   & {:keys [color fill-color width]
      :or {color :black fill-color nil width 1.0}}]
  (stroke-width width ctx)
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

(defn text-width
  "Find the width of text as it would be rendered to the canvas."
  [text ctx &
   {:keys [size] :or {size 14}}]
  (set! (.-font ctx) (str size "px sans"))
  (let [res (.measureText ctx text)]
    (.-width res)))

(defn text
  "Draw text to the canvas."
  [text [x y] ctx &
   {:keys [fill-color size align]
    :or {fill-color :black size 14 align :left}}]
  (set! (.-font ctx) (str size "px sans"))
  (set! (.-textBaseline ctx) "top")
  (set! (.-textAlign ctx) (name align))
  (set! (.-lineJoin ctx) "round")
  (stroke-width 3 ctx)
  (stroke-style :white ctx)
  (.strokeText ctx text x y)
  (fill-style fill-color ctx)
  (.fillText ctx text x y)
  (set! (.-lineJoin ctx) "miter"))
