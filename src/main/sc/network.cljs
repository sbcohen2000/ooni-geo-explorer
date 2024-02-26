(ns sc.network
  (:require [cljs.core.async :refer [chan put!]]
            [clojure.string :as str]))

(defn query-string
  "Return the query string portion of a url based on the given map, `kvs`"
  [kvs]
  (str "?" (str/join "&" (map (fn [[k v]] (str k "=" v)) kvs))))

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
