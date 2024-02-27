(ns sc.rect
  (:require [sc.vector2 :as v]))

(defn from-bounds
  [left top right bottom]
  [left top (- right left) (- bottom top)])

(defn x
  [[x _ _ _]]
  x)

(defn y
  [[_ y _ _]]
  y)

(defn width
  [[_ _ w _]]
  w)

(defn height
  [[_ _ _ h]]
  h)

(defn left
  [[x _ _ _]]
  x)

(defn right
  [[x _ w _]]
  (+ x w))

(defn top
  [[_ y _ _]]
  y)

(defn bottom
  [[_ y _ h]]
  (+ y h))

(defn upper-left
  [[x y _ _]] [x y])

(defn offset
  "Offset the rectangle by the given vector"
  [[x y w h] [ox oy]]
  [(+ x ox) (+ y oy) w h])

(defn center
  "Find the center point of the rectangle."
  [[x y w h]]
  (v/+ [x y] (v/* [w h] 0.5)))

(defn scale-to-fit
  "Scale the rectangle such that it fits within a box
  of size `w'`x`h'`."
  [[x y w h] w' h']
  (let [scale (min (/ w' w) (/ h' h))]
    [x y (* scale w) (* scale h)]))

(defn intersection?
  "Check if two rectangles overlap."
  [a b]
  (and (< (left a) (right b))
       (< (left b) (right a))
       (< (top a) (bottom b))
       (< (top b) (bottom a))))

(defn includes?
  "Check if a point is contained within the rectangle."
  [[x y w h] [px py]]
  (and (>= px x)
       (>= py y)
       (< px (+ x w))
       (< py (+ y h))))

(defn area
  "Find the area of a rectangle."
  [[_ _ w h]]
  (* w h))
