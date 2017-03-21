(ns roomlist.events
  (:require
    [reagent.core :as r]
    [taoensso.sente :as sente :refer (cb-success?)]
    [re-frame.core :as re-frame]
    [roomlist.db :as mydb]))

; generate a list of usernames
(def possible-usernames
  (let [first-names ["Happy" "Gleeful" "Joyful" "Cheerful" "Merry" "Jolly"]
        last-names ["Dan" "John" "Larry" "Thinh" "Quang" "Dung"]]
    (for [name1 first-names
          name2 last-names]
      (str name1 " " name2))))

; sente js setup
(def ws-connection (sente/make-channel-socket! "/channel" {:type :auto}))
(let [{:keys [ch-recv send-fn]}
      ws-connection]
  (def receive-channel (:ch-recv ws-connection))
  (def send-channel! (:send-fn ws-connection)))


;; event handlers
(re-frame/reg-event-db
  :initialize-db
  (fn  [_ _]
    mydb/default-db))

(re-frame/reg-event-db
  :set-usernames
  (fn [db [_ data]]
    ;(println (str "data: " data))
    (assoc db :user/names data)))

(defn updatetable [tableconfig changeData]
  (let [dataTable (get-in tableconfig [:data])
        newDataTable (assoc-in dataTable (subvec changeData 0 2) (js/parseFloat (nth changeData 3)))
        tableconfig (assoc-in tableconfig [:data] newDataTable)]
    (send-channel! [:user/set-table-value changeData])
    tableconfig))

(re-frame/reg-event-db
  :set-tablevalue
  (fn [db [_ inchangeDatas]]
    (let [changeDatas (js->clj inchangeDatas)
          tableconfig (get-in db [:tableconfig])
          newtableconfig (if (nil? changeDatas) nil (reduce updatetable tableconfig changeDatas))
          dbout (if (nil? newtableconfig) db (assoc db :tableconfig newtableconfig))]
      (if (some? changeDatas) (do
                                (println "set-tablevalue")
                                (println changeDatas)))
                                ;(println dbout)))
      db)))

(re-frame/reg-event-db
  :settableload
  (fn [db [_ _]]
    (assoc db :tableload (rand))))

(re-frame/reg-event-db
  :set-tableconfig
  (fn [db [_ tableconfig]]
    (let [newdb (assoc db :tableconfig tableconfig)]
      ;(println "set-tableconfig: " tableconfig)
      ;(println "old-db: " db)
      ;(println "new-db: " newdb)
      newdb)))

; handle application-specific events
(defn- app-message-received [[msgType data]]
  (case msgType
    :user/names (re-frame/dispatch [:set-usernames data]) ; (reset! events {:data data})
    :db/table (do
                (re-frame/dispatch [:settableload])
                (re-frame/dispatch [:set-tableconfig data]))
    (.log js/console "Unmatched application event")))

; handle websocket-connection-specific events
(defn- channel-state-message-received [state]
  (if (:first-open? state)
    (send-channel! [:user/ident {:name (rand-nth possible-usernames)}])))

; main router for websocket events
(defn- event-handler [[id data] _]
  (.log js/console "received message" data)
  (case id
    :chsk/state (channel-state-message-received data)
    :chsk/recv (app-message-received data)
    (.log js/console "Unmatched connection event")))

(sente/start-chsk-router-loop! event-handler receive-channel)