(ns roomlist.core
  (:require [reagent.core :as reagent]
            [re-frame.core :as re-frame]
            [roomlist.events]
            [roomlist.subs]
            [roomlist.system :as push]
            [roomlist.views :as views]
            [roomlist.config :as config]))

(defn dev-setup []
  (when config/debug?
    (enable-console-print!)
    (println "dev mode")))

(defn mount-root []
  (re-frame/clear-subscription-cache!)
  (reagent/render [views/main-panel]
                  (.getElementById js/document "entry-list")))

(defn ^:export init []
  (re-frame/dispatch-sync [:initialize-db])
  (dev-setup)
  (mount-root))