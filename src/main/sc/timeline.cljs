(ns sc.timeline)

(defn model
  "Return the initial state of the timeline."
  []
  {})

(defn handler
  "Update the state given an event."
  [state [tag props]]
  state)

(defn paint
  "Paint the timeline to the canvas given its current state."
  [state ctx])
