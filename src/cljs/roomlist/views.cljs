(ns roomlist.views
  (:require [reagent.core :as reagent]
            [reagent.ratom :as ratom]
            [re-frame.core :refer [subscribe dispatch]]))

(defn gen-chart-config-handson
  []
  (let [currenttableconfig  (subscribe [:tableconfig])
        ret (reagent/atom {
                           :chart    {:type     "line"
                                      :zoomType "xy"}
                           :title    {:text "Directional Survey"}
                           :subtitle {:text "An experiment"}
                           :xAxis    {:title      {:text "X"}}
                           :yAxis    {:title      {:text "Y"}
                                      :reversed true}
                           :credits  {:enabled false}})]
    (let [tabledata (:data @currenttableconfig)
          tmptabledata (into [[0 0 0]] tabledata)
          tmptabledata1 (mapv (fn [in]
                                (let [md (get in 0)
                                      tvd (get in 1)
                                      dev (get in 2)]
                                  [md tvd dev 0]))
                              tmptabledata)
          tmptable (reduce (fn [data rowIdx]
                             (let [md1 (get-in data [(- rowIdx 1) 0])
                                   md2 (get-in data [rowIdx 0])
                                   x1 (get-in data [(- rowIdx 1) 3])
                                   dev2 (get-in data [rowIdx 2])
                                   x2 (+ x1 (* (- md2 md1) (js/Math.sin (* (/ dev2 180.0) js/Math.PI))))]
                               (assoc-in data [rowIdx 3] x2)))
                           tmptabledata1
                           (range 1 (count tmptabledata1)))
          tmptable1 (rest tmptable)
          gendata (mapv (fn [data]
                         (let [y (get data 1)
                               x (get data 3)]
                           [x y]))
                        tmptable1)
          mydata [{:name "Directional survey" :data gendata}]]
      ;(println "currenttableconfig: " tabledata)
      ;(println "tmptabledata: " tmptabledata)
      ;(println "tmptabledata1: " tmptabledata1)
      ;(println "tmptable: " tmptable)
      ;(println "mydata: " mydata)
      (swap! ret assoc-in [:series] mydata))
    ret))

(defn sampleHighchart-render []
  [:div {:style {:min-width "310px" :max-width "800px" :margin "0 auto"}}])

(defn sampleHighchart-did-mount [this]
  (let [[_ tableconfig] (reagent/argv this)
        my-chart-config (gen-chart-config-handson)]
    (js/Highcharts.Chart. (reagent/dom-node this) (clj->js @my-chart-config))))

(defn sampleHighchart-did-update [this]
  (let [[_ tableconfig] (reagent/argv this)
        my-chart-config (gen-chart-config-handson)]
    (do
      (js/Highcharts.Chart. (reagent/dom-node this) (clj->js @my-chart-config)))))

(defn sampleHighchart [tableconfig]
  (reagent/create-class {:reagent-render      sampleHighchart-render
                         :component-did-mount sampleHighchart-did-mount
                         :component-did-update sampleHighchart-did-update}))

(defn sampleHighchartWrapper []
  (let [tableconfig (subscribe [:tableconfig])
        tableload (subscribe [:tableload])]
    [:div "My Chart"
     [sampleHighchart @tableconfig @tableload]]))

(defn sampleTable [tableconfig]
  (let [table (atom nil)
        tmp @(subscribe [:tableconfig])]
    (reagent/create-class {:reagent-render
                           (fn [] [:div {:style {:min-width "310px" :max-width "800px" :margin "0 auto"}}])
                           :component-did-mount
                           (fn [this]
                             (do
                               (reset! table (js/Handsontable (reagent/dom-node this) (clj->js (assoc-in @tableconfig [:afterChange] #(dispatch [:set-tablevalue %])))))))
                           :should-component-update
                           (fn [this [_ old-config] [_ new-config]]
                             ;(println "should-component-update: " @tableconfig)
                             ;(println "should-component-update: " (assoc-in @tableconfig [:afterChange] #(dispatch [:set-tablevalue %])))
                             (.destroy @table)
                             (reset! table (js/Handsontable (reagent/dom-node this) (clj->js (assoc-in @tableconfig [:afterChange] #(dispatch [:set-tablevalue %])))))
                             true)})))

(defn sampleTableWrapper []
  (let [tableconfig (subscribe [:tableconfig])
        usernames (subscribe [:user/names])
        tableload (subscribe [:tableload])]
    [:div "My Table"
     [sampleTable tableconfig @tableload]]))

(defn user-item [name]
  [:li (str (:user/name name))])

(defn users-list []
  (let [usernames @(subscribe [:user/names])]
    [:ul
     (cond
       (= nil usernames) [:li "There is no users"]
       :else (for [name usernames]
                ^{:key name} [user-item {:user/name name}]))]))

(defn main-panel []
  (let [name (subscribe [:name])]
    (fn []
      [:div "Hello from " @name ". Vui qua ta"
       [users-list]
       [sampleTableWrapper]
       [sampleHighchartWrapper]])))