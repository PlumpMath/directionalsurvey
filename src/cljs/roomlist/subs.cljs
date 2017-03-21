(ns roomlist.subs
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [re-frame.core :as re-frame]
            [roomlist.db :as mydb]))

(re-frame/reg-sub
  :name
  (fn [db]
    (:name db)))

(re-frame/reg-sub
  :user/names
  (fn [db]
    (:user/names db)))

(re-frame/reg-sub
  :tableload
  (fn [db _]
    (get-in db [:tableload])))

(re-frame/reg-sub
  :tableconfig
  (fn [db _]
    (get-in db [:tableconfig] mydb/init-tableconfig)))
