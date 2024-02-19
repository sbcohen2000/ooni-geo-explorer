(ns sc.network
  (:require [cljs.core.async :refer [chan put!]]))

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
