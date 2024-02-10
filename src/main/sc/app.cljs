(ns sc.app
  (:require [sc.canvas]))

(defn on-resize
  []
  (sc.canvas/resize-canvas (sc.canvas/getContext)))

(defn setup-resize-handler
  []
  (.removeEventListener js/window "resize" on-resize)
  (.addEventListener js/window "resize" on-resize))

(defn init!
  []
  (setup-resize-handler)
  (println "init!"))

(defn reload!
  []
  (setup-resize-handler)
  (println "reload!"))
