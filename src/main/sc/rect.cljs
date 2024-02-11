(ns sc.rect)

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

(defn scale
  [[x y w h] s]
  [(* s x) (* s y) (* s w) (* s h)])
