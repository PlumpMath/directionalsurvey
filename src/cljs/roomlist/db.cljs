(ns roomlist.db)


(def init-tableconfig {
                       :colHeaders ["MD" "TVD" "Deviation"]
                       :data        [
                                     [0 0 0]
                                     [0 0 0]
                                     [0 0 0]]
                       :rowHeaders  false
                       :contextMenu true})

(def default-db
  {:name "re-frame"
   :tableload (rand)
   :tableconfig {
                 :colHeaders ["MD" "TVD" "Deviation"]
                 :data        [
                               [0 0 0]
                               [0 0 0]
                               [0 0 0]]
                 :rowHeaders  false
                 :contextMenu true}})