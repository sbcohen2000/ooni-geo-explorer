(ns sc.map
  (:require [cljs.core.async :refer [<! go put!]]
            [sc.canvas]
            [sc.network]
            [sc.rect]
            [sc.vector2 :as v]))

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

(defn polygon-bbox
  "Calculate the bounding box of a polygon."
  [points]
  (let [all-points (mapcat concat points)
        bounds (reduce
                (fn [[l t r b] [x y]]
                  [(min x l) (min y t) (max x r) (max y b)])
                [pos-inf pos-inf neg-inf neg-inf] all-points)]
    (apply sc.rect/from-bounds bounds)))

(defn geometry-bbox
  [geometry]
  (apply max-key sc.rect/area (map polygon-bbox geometry)))

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
  (go
    (let [data (->> (<! (sc.network/fetch-json "./geo-data.json"))
                    (map (fn [[cc v]]
                           [cc (update v :geometry project-geometry)]))
                    (map (fn [[cc v]]
                           [cc (assoc v :bbox (geometry-bbox (:geometry v)))]))
                    (#(into {} %)))]
      (println "Loaded geo data!")
      (put! event-stream [:repaint [:map #(assoc % :geo-data data :visible-ccs (set (keys data)))]])))
  {:src              [min-lon min-lat (- max-lon min-lon) (- max-lat min-lat)] ;; in degrees
   :drag-offset      nil ;; in degrees
   :w                0
   :h                0
   :visible-ccs      #{}
   :geo-data         nil
   :event-stream     event-stream
   :pie-size         50})

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
                   (:bbox (% (:geo-data state))))
                 (keys (:geo-data state))))))

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
         (sc.canvas/polygon outer-ring proj ctx :fill-color :linen))
       (doseq [hole (rest polygon)]
         (sc.canvas/polygon hole proj ctx :fill-color :linen))))))

(defn draw-visible-country-labels
  "Draw country labels for the entire visible map."
  [state ctx]
  (let [proj    (degrees-to-px (:src state) (dst state))
        h-scale (/ (sc.rect/width (dst state)) (sc.rect/width (:src state)))]
    (doseq [cc (:visible-ccs state)]
      (let [data (cc (:geo-data state))
            full-name-width (sc.canvas/text-width (:country_name data) ctx)
            cc-name-width   (sc.canvas/text-width (name cc) ctx)
            bbox-width      (sc.rect/width  (:bbox data))
            bbox-width-px   (* bbox-width h-scale)
            center          (sc.rect/center (:bbox data))
            center-px       (proj center)]
        (cond
          (< full-name-width bbox-width-px)
          (sc.canvas/text (:country_name data) center-px ctx :align :center)

          (< cc-name-width bbox-width-px)
          (sc.canvas/text (name cc) center-px ctx :align :center)

          :else
          nil)))))

(defn draw-visible-ccs
  "Draw the entire visible map"
  [state ctx]
  (sc.canvas/clear ctx)
  (sc.canvas/rectangle (dst state) ctx :fill-color :lightskyblue :color nil)
  (doseq [cc (:visible-ccs state)]
    (let [data (cc (:geo-data state))]
      (draw-geometry
       (:geometry data)
       (:src state)
       (dst state)
       ctx))))

(defn draw-pie
  "Draw a pie chart centered at `center` and with radius `r`,
  with the probabilities `ps`. `ps` is a list of tuples,
  [probability, color], and the probabilities in `ps` are assumed to
  sum to 1."
  [center r ctx & ps]
  (reduce (fn [s [p color]]
            (sc.canvas/arc center r s (+ s (* p tau)) ctx
                           :fill-color color
                           :color nil)
            (+ s (* p tau))) 0 ps))

(defn draw-pies
  "Draw pie charts from the current datapoint."
  [state data ctx]
  (let [proj (degrees-to-px (:src state) (dst state))
        max-total (apply max (map :measurement_count data))]
    (doseq [elem data]
      (let [cc (keyword (:probe_cc elem))
            geo-data  (cc (:geo-data state))
            center    (sc.rect/center (:bbox geo-data))
            center-px (proj center)

            total       (:measurement_count  elem)
            p-anomaly   (/ (:anomaly_count   elem) total)
            p-confirmed (/ (:confirmed_count elem) total)
            p-failure   (/ (:failure_count   elem) total)
            p-ok        (/ (:ok_count        elem) total)
            p-total     (/ total max-total)
            r           (max (min (* (js/Math.sqrt p-total) (:pie-size state)) 300) 5)]
        (draw-pie center-px r ctx
                  [p-ok        :seagreen]
                  [p-anomaly   :darkorange]
                  [p-confirmed :crimson]
                  [p-failure   :gray])))))

(defn paint
  "Paint the map to the canvas given its state and a datapoint."
  [state data ctx]
  (draw-visible-ccs state ctx)
  (draw-visible-country-labels state ctx)
  (when data
    (draw-pies state data ctx)))
