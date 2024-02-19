(ns sc.map
  (:require [cljs.core.async :refer [<! go close!]]
            [sc.canvas]
            [sc.network]
            [sc.rect]
            [sc.vector2 :as v]))

(defonce geo-data (atom nil))

(def pi js/Math.PI)
(def tau (* 2 js/Math.PI))
(def pi-4 (/ pi 4))
(def pi-2 (/ pi 2))

(defn degs-to-rads
  [angle]
  (/ (* pi angle) 180))

(defn rads-to-degs
  [angle]
  (/ (* 180 angle) pi))

(defn mercator
  "Mercator projection of the latitude and longitude into pixel coordinates.
  "
  [lon lat]
  (let [;; radius of earth is found such that the circumference of
        ;; the globe matches the width of the view-port.
        r (/ 360 tau)
        y (* r (js/Math.log (js/Math.tan (+ pi-4 (/ (degs-to-rads lat) 2)))))
        x (* r (degs-to-rads lon))]
    [(+ x 180) (- 180 y)]))

(defn inverse-mercator
  "Inverse Mercator projection of the x and y pixel coordiantes into
  latitude and longitude.
  "
  [x y]
  (let [r (/ 360 tau)
        x' (- x 180)
        y' (- y 180)
        lat (rads-to-degs (- (* 2 (js/Math.atan (js/Math.exp (/ y' r)))) pi-2))
        lon (rads-to-degs (/ x' r))]
    [lon lat]))

(defn degrees-to-px
  [src dst]
  (let [h-scale (/ (sc.rect/width dst) (sc.rect/width src))
        v-scale (/ (sc.rect/height dst) (sc.rect/height src))]
    (fn [v]
      (let [[x' y'] (v/- v (sc.rect/upper-left src))]
        [(* x' h-scale) (* y' v-scale)]))))

(defn px-to-degrees
  [src dst]
  (let [h-scale (/ (sc.rect/width dst) (sc.rect/width src))
        v-scale (/ (sc.rect/height dst) (sc.rect/height src))]
    (fn [[x y]]
      (let [v' [(/ x h-scale) (/ y v-scale)]]
        (v/+ v' (sc.rect/upper-left src))))))

(defn project-geometry
  [geometry]
  (map
   (fn [polygon]
     (map
      (fn [ring]
        (map
         (fn [[lon lat]] (mercator lon lat))
         ring))
      polygon))
   geometry))

(defn model
  "Return the initial state of the map."
  []
  (when-not @geo-data
    (go
      (let [data (<! (sc.network/fetch-json "./geo-data.json"))
            data-proj (into {} (map (fn [[cc v]]
                                      [cc (update v :geometry project-geometry)])
                                    data))]
        (println "Loaded geo data!")
        (reset! geo-data data-proj))))
  {:src [0 0 360 360] ;; in degrees
   :drag-offset nil   ;; in degrees
   :w 0
   :h 0})

(defn dst
  "Get the rendering destination rectangle."
  [state]
  [0 0 (:w state) (:h state)])

(defn zoom
  "Return a new rectangle which is scaled by the given amount `s`,
  holding the relative distance to `p` from each edge constant."
  [rect p s]
  (let [[x y w h] rect
        [px py] p
        w' (* w s)
        h' (* h s)]
    [(- px (* s (- px x)))
     (- py (* s (- py y))) w' h']))

(defn handler
  "Update the state given an event."
  [state [tag props]]
  (case tag
    :resize (assoc state :w (:w props) :h (:h props))

    :click (let [dst [0 0 (:w state) (:h state)]
                 proj (px-to-degrees (:src state) dst)]
             (println (proj (:p props)))
             state)

    :drag-start
    (assoc state :drag-offset (sc.rect/upper-left (:src state)))

    :drag-end
    (assoc state :drag-offset nil)

    :drag
    (let [proj (px-to-degrees (:src state) (dst state))
          ofs (v/- (proj (:initial props)) (proj (:p props)))
          [x y] (v/+ ofs (:drag-offset state))]
      (update state :src #(vector x y (sc.rect/width %) (sc.rect/height %))))

    :wheel
    (let [scale (+ 1.0 (* (:dir props) 0.1))
          proj (px-to-degrees (:src state) (dst state))]
      (update state :src #(zoom % (proj (:p props)) scale)))

    state))

(defn draw-geometry
  "Draw a country's border geometry from the rectangle src into the destination dst.

  See [[mercator]].
  "
  [geometry src dst ctx]
  (let [proj (degrees-to-px src dst)]
    (doseq [polygon geometry]
     (when (seq polygon)
       (let [outer-ring (first polygon)]
         (sc.canvas/polygon outer-ring proj ctx))
       (doseq [hole (rest polygon)]
         (sc.canvas/polygon hole proj ctx))))))

(defn draw-all
  "Draw the entire map"
  [src dst ctx]
  (sc.canvas/clear ctx)
  (doseq [cc (keys @geo-data)]
    (draw-geometry (:geometry (cc @geo-data)) src dst ctx)))

(defn paint
  "Paint the map to the canvas given its state."
  [state ctx]
  (draw-all (:src state) [0 0 (:w state) (:h state)] ctx))
