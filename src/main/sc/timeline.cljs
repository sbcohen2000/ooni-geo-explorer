(ns sc.timeline
  (:require [sc.canvas]))

(def ms-in-year  3.154e+10)
(def ms-in-month 2.628e+9)
(def ms-in-day   8.64e+7)
(def ms-in-hour  3.6e+6)

(defn model
  "Return the initial state of the timeline."
  []
  {:w 0
   :h 0
   :from (- (js/Date.now) (* 0.5 ms-in-month))
   :to   (js/Date.now)})

(defn find-appropriate-time-scale
  [from to]
  (let [dt (abs (- to from))]
    (cond
      (< dt (* 2 ms-in-day))   :hour
      (< dt (* 2 ms-in-month)) :day
      (< dt (* 2 ms-in-year))  :month
      :else :year)))

(defn create-time-scale
  [from to]
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

(defn handler
  "Update the state given an event."
  [state [tag props]]
  (case tag
    :resize (-> state
                (assoc :w (:w props) :h (:h props)))
    state))

(defn paint-time-scale
  [state ctx]
  (let [scale (create-time-scale (:from state) (:to state))
        height (:h state)
        x-scale (/ (:w state) (- (:to state) (:from state)))]
    (doseq [time scale]
      (let [x (* x-scale (- time (:from state)))
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
