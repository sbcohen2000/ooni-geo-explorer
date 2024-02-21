(ns sc.timeline
  (:require [sc.canvas]))

(def ms-in-year  3.154e+10)
(def ms-in-month 2.628e+9)
(def ms-in-day   8.64e+7)
(def ms-in-hour  3.6e+6)

(defn model
  "Return the initial state of the timeline."
  []
  {:w           0
   :h           0
   :src         [(- (js/Date.now) (* 0.5 ms-in-month)) (js/Date.now)]
   :drag-offset nil})

(defn find-appropriate-time-scale
  [from to]
  (let [dt (abs (- to from))]
    (cond
      (< dt (* 2 ms-in-day))   :hour
      (< dt (* 2 ms-in-month)) :day
      (< dt (* 2 ms-in-year))  :month
      :else :year)))

(defn create-time-scale
  [[from to]]
  (let [from-obj    (new js/Date from)
        from-hours (.getUTCHours    from-obj)
        from-days  (.getUTCDate     from-obj)
        from-month (.getUTCMonth    from-obj)
        from-year  (.getUTCFullYear from-obj)
        time-scale (find-appropriate-time-scale from to)
        start-time (case time-scale
                     :hour  (js/Date.UTC from-year from-month from-days from-hours)
                     :day   (js/Date.UTC from-year from-month from-days)
                     :month (js/Date.UTC from-year from-month)
                     :year  (js/Date.UTC from-year))
        dt (case time-scale
             :hour  ms-in-hour
             :day   ms-in-day
             :month ms-in-month
             :year  ms-in-year)
        n-points (js/Math.ceil (/ (- to from) dt))]
    (map #(+ start-time (* % dt)) (range 0 n-points))))

(defn dst
  "Get the rendering destination rectangle."
  [state]
  [0 0 (:w state) (:h state)])

(defn ensure-bounded
  [src]
  (let [[from to] src
        w (- to from)]
    (-> src
        (#(if (> w (* 20 ms-in-year))
            [(- (second %) (* 20 ms-in-year)) (second %)]
            %))
        (#(if (> to (js/Date.now))
            (let [d (- to (js/Date.now))]
              [(- (first %) d) (- (second %) d)])
            %)))))

(defn update-src-if-valid
  "Update the from/to source range with the given function,
  ensuring that the source range is valid after the transformation."
  [state f]
  (let [f' #(ensure-bounded (f %))]
    (update state :src f')))

(defn px-to-ms
  "Convert a value in pixels to milliseconds according to the current
  source range.e"
  [state x]
  (let [[from to] (:src state)
        x-scale (/ (:w state) (- to from))]
    (js/Math.floor (+ (/ x x-scale) from))))

(defn ms-to-px
  "Convert a value from milliseconds to pixels according to the current
  source range."
  [state ms]
  (let [[from to] (:src state)
        x-scale (/ (:w state) (- to from))]
    (* x-scale (- ms from))))

(defn zoom
  "Return a new source range which is scaled by the given amount `s`,
  holding the relative distance to `x` from each edge constant."
  [range x s]
  (let [[from to] range
        w'        (* (- to from) s)
        from'     (- x (* s (- x from)))]
    [from' (+ from' w')]))

(defn handler
  "Update the state given an event."
  [state [tag props]]
  (case tag
    :resize (-> state
                (assoc :w (:w props) :h (:h props)))

    :drag-start
    (assoc state :drag-offset (:src state))

    :drag-end
    (assoc state :drag-offset nil)

    :drag
    (let [cur-ms     (px-to-ms state (first (:p props)))
          initial-ms (px-to-ms state (first (:initial props)))
          ofs        (- initial-ms cur-ms)
          src'       (map (partial + ofs) (:drag-offset state))]
      (update-src-if-valid state (constantly src')))

    :wheel
    (let [scale (+ 1.0 (* (:dir props) 0.1))
          x     (px-to-ms state (first (:p props)))]
      (update-src-if-valid state #(zoom % x scale)))

    state))

(defn paint-time-scale
  [state ctx]
  (let [scale     (create-time-scale (:src state))
        height    (:h state)]
    (doseq [time scale]
     (let [x (ms-to-px state time)
           str (.toDateString (new js/Date time))]
       (sc.canvas/with-offset [x height] ctx
         (sc.canvas/with-rotation 0.8 ctx
           (sc.canvas/text str [0 0] ctx)))
       (sc.canvas/line [x 0] [x height] ctx)))))

(defn paint
  "Paint the timeline to the canvas given its current state."
  [state ctx]
  (sc.canvas/rectangle (dst state) ctx :fill-color :white :color :red)
  (paint-time-scale state ctx))
