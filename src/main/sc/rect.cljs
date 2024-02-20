(ns sc.rect)

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

(defn scale
  ([[x y w h] s]
   [(* s x) (* s y) (* s w) (* s h)])
  ([[x y w h] sx sy]
   [(* sx x) (* sy y) (* sx w) (* sy h)]))

(defn shrink
  ([[x y w h] s]
   (let [w' (* w s)
         h' (* h s)]
     [(+ x (/ w' 2)) (+ y (/ h' 2)) w' h'])))

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
