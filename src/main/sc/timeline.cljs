(ns sc.timeline
  (:require [sc.canvas]))

(defn model
  "Return the initial state of the timeline."
  []
  {:w 0
   :h 0})

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

(defn paint
  "Paint the timeline to the canvas given its current state."
  [state ctx]
  (sc.canvas/rectangle (dst state) ctx))
