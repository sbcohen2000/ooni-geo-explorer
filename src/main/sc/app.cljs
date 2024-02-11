(ns sc.app
  (:require
   [sc.canvas]
   [cljs.core.async :refer [chan put! go <! >!]]))

(defonce geo-data (atom []))

(defonce on-resize
  (fn []
   (sc.canvas/resize-canvas (sc.canvas/getContext))))

(defn setup-resize-handler
  []
  (.removeEventListener js/window "resize" on-resize)
  (.addEventListener js/window "resize" on-resize))

(defn fetch-json
  "Request the JSON resource at the given URL, and return a channel
  which will receive a Clojure dictionary representing the JSON
  response."
  [url]
  (let [response (chan)]
    (-> (js/fetch url)
      (.then #(.json %))
      (.then #(js->clj % :keywordize-keys true))
      (.then #(put! response %)))
    response))

(defn draw-geometry
  "Draw a country's border geometry."
  [geometry w h ctx]
  (doseq [polygon geometry]
    (when (seq polygon)
      (let [outer-ring (first polygon)]
        (sc.canvas/polygon outer-ring w h ctx)))
    (doseq [hole polygon]
      (sc.canvas/polygon hole w h ctx))))

(defn draw-all
  "Draw the entire map"
  []
  (let [ctx (sc.canvas/getContext)
        size 500]
    (sc.canvas/clear ctx)
    (doseq [cc (keys @geo-data)]
     (draw-geometry (:geometry (cc @geo-data)) size size ctx))))

(defn doit
  []
  (go (let [data (<! (fetch-json "./geo-data.json"))]
        (reset! geo-data data))))

(defn init!
  []
  (setup-resize-handler)
  (println "init!"))

(defn reload!
  []
  (setup-resize-handler)
  (println "reload!"))
