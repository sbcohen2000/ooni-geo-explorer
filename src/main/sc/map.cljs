(ns sc.map
  (:require [cljs.core.async :refer [<! go put!]]
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

(def pos-inf js/Number.POSITIVE_INFINITY)
(def neg-inf js/Number.NEGATIVE_INFINITY)

(defn geometry-bbox
  [geometry]
  (let [all-points (mapcat (fn [ring] (mapcat concat ring)) geometry)
        bounds (reduce
                (fn [[l t r b] [x y]]
                  [(min x l) (min y t) (max x r) (max y b)])
                [pos-inf pos-inf neg-inf neg-inf] all-points)]
    (apply sc.rect/from-bounds bounds)))

(defn ensure-aspect
  "Ensure that the input rectangle has the given aspect ratio."
  [[x y w _] a]
  [x y w (* w a)])

(def min-lon 0)
(def max-lon 360)
(def min-lat 30)
(def max-lat 320)

(defn ensure-bounded
  "Modify the input rect to fit within the bounds of the map."
  [rect]
  (let [w (- max-lon min-lon)
        h (- max-lat min-lat)]
    (-> rect
        (#(if (or (> (sc.rect/width %) w)
                  (> (sc.rect/height %) h))
            (sc.rect/scale-to-fit % w h) %))
        (#(if (> (sc.rect/right %) max-lon)
            (sc.rect/offset % [(- max-lon (sc.rect/right %)) 0])
            %))
        (#(if (< (sc.rect/left %) min-lon)
            (sc.rect/offset % [(- min-lon (sc.rect/left %)) 0])
            %))
        (#(if (< (sc.rect/top %) min-lat)
            (sc.rect/offset % [0 (- min-lat (sc.rect/top %))])
            %))
        (#(if (> (sc.rect/bottom %) max-lat)
            (sc.rect/offset % [0 (- max-lat (sc.rect/bottom %))])
            %)))))

(defn model
  "Return the initial state of the map."
  [event-stream]
  (when-not @geo-data
    (go
      (let [data (->> (<! (sc.network/fetch-json "./geo-data.json"))
                      (map (fn [[cc v]]
                             [cc (update v :geometry project-geometry)]))
                      (map (fn [[cc v]]
                             [cc (assoc v :bbox (geometry-bbox (:geometry v)))])))]
        (println "Loaded geo data!")
        (reset! geo-data (into {} data)))))
  {:src              [min-lon min-lat (- max-lon min-lon) (- max-lat min-lat)] ;; in degrees
   :drag-offset      nil ;; in degrees
   :w                0
   :h                0
   :visible-ccs      #{}
   :event-stream     event-stream})

(defn invalidate!
  [state]
  (put! (:event-stream state) [:invalidate]))

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

(defn get-visible-ccs
  "Return a set of visible country codes given the map state."
  [state]
  (let [rect (:src state)]
    (set (filter #(sc.rect/intersection?
                   rect
                   (:bbox (% @geo-data)))
                 (keys @geo-data)))))

(defn update-src-if-valid
  "Update the source rect with the given function, ensuring that the src
  rect is valid after the transformation.

  Update visible-ccs given the new src rect."
  [state f]
  (let [f' #(ensure-bounded (f %))
        with-src (update state :src f')
        new-ccs (get-visible-ccs with-src)]
    ;; Check if the set of visible countries has changed.
    ;; If so, we need to invalidate!.
    (if (= new-ccs (:visible-ccs with-src))
      with-src
      (let [with-ccs (assoc with-src :visible-ccs new-ccs)]
        (invalidate! with-ccs)
        with-ccs))))

(defn handler
  "Update the state given an event."
  [state [tag props]]
  (case tag
    :resize (-> state
                (assoc :w (:w props) :h (:h props))
                (update-src-if-valid #(ensure-aspect % (/ (:h props) (:w props)))))

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
      (update-src-if-valid state #(vector x y (sc.rect/width %) (sc.rect/height %))))

    :wheel
    (let [scale (+ 1.0 (* (:dir props) 0.1))
          proj (px-to-degrees (:src state) (dst state))]
      (update-src-if-valid state #(zoom % (proj (:p props)) scale)))

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

(defn draw-visible-ccs
  "Draw the entire visible map"
  [state ctx]
  (sc.canvas/clear ctx)
  (doseq [cc (:visible-ccs state)]
    (draw-geometry
     (:geometry (cc @geo-data))
     (:src state)
     (dst state)
     ctx)))

(defn paint
  "Paint the map to the canvas given its state."
  [state ctx]
  (draw-visible-ccs state ctx))
