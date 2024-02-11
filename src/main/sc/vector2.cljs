(ns sc.vector2
  (:require [cljs.core])
  (:refer-clojure :exclude [+ - *]))

(def epsilon 0.00001)

(defn nearly-zero?
  "Test if a number is nearly zero."
  [n]
  (< (abs n) epsilon))

(defn -
  "Subtract two vectors."
  [[x1 y1] [x2 y2]]
  [(cljs.core/- x1 x2) (cljs.core/- y1 y2)])

(defn +
  "Add two vectors."
  [[x1 y1] [x2 y2]]
  [(cljs.core/+ x1 x2) (cljs.core/+ y1 y2)])

(defn *
  "Multiply a vector by a scalar."
  [[x y] t]
  [(cljs.core/* x t) (cljs.core/* y t)])

(defn magnitude
  "Get the length of a vector."
  [[x y]]
  (js/Math.sqrt (cljs.core/+ (cljs.core/* x x) (cljs.core/* y y))))

(defn squared-magnitude
  "Get the squared magnitude of a vector."
  [[x y]]
  (cljs.core/+ (cljs.core/* x x) (cljs.core/* y y)))

(defn distance
  "Get the distance between two vectors."
  [a b]
  (magnitude (- a b)))

(defn squared-distance
  "Get the squared distance between two vectors."
  [a b]
  (squared-magnitude (- a b)))

(defn approx-eq?
  "Test if two vectors are approximately equal."
  [a b]
  (nearly-zero? (squared-distance a b)))

(defn normalize
  "Normalize a vector."
  [v]
  (let [m (magnitude v)]
    (if (nearly-zero? m)
      v
      (* v (/ 1 m)))))

(defn cross
  "Find the cross product of two vectors."
  [[x1 y1] [x2 y2]]
  (cljs.core/- (cljs.core/* x1 y2) (cljs.core/* y1 x2)))

(defn dot
  "Find the dot product of two vectors in."
  [[x1 y1] [x2 y2]]
  (cljs.core/+ (cljs.core/* x1 x2) (cljs.core/* y1 y2)))

(defn proj
  "Project a onto b."
  [a b]
  (* b (/ (dot a b) (dot b b))))

(defn ortho-proj
  "Find the vector component of a which is orthogonal to b."
  [a b]
  (- a (proj a b)))

(defn intersection
  "Find the intersection of two line segments, or nil if the lines do
  not intersect. Returns nil if the lines are co-linear."
  [[a b] [c d]]
  (let [p a        ; start point of first line
        r (- b p) ; a vector pointing to endpoint of first line
        q c        ; end point of second line
        s (- d q) ; a vector pointing to endpoint of second line
        rs (cross r s)]
    (if (nearly-zero? rs) nil
        (let [t (/ (cross (- q p) s) rs)
              u (/ (cross (- q p) r) rs)]
          (if (and
               (>= t epsilon)
               (<= t (cljs.core/- 1 epsilon))
               (>= u epsilon)
               (<= u (cljs.core/- 1 epsilon)))
            (+ p (* r t))
            nil)))))

(defn on-edge?
  "Check if a point is on the given edge."
  [[a b] p]
  (let [s (- b a)
        r (- p a)
        c (abs (cross s r)) ; if c is close to zero, c is on the line
        d (dot s r)         ; if 0 < d < 1, the point is on the segment
        l (squared-magnitude (- b a))]
    (and (< c epsilon) (> d 0) (< d l))))
