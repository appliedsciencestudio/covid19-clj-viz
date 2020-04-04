(ns appliedsciencestudio.covid19-clj-viz.world
  (:require [jsonista.core :as json]
            [oz.core :as oz]
            [appliedsciencestudio.covid19-clj-viz.common :refer [oz-config
                                                                 applied-science-palette]]))

(comment
  (oz/start-server! 8082))

(defonce covid19world-json
         (slurp "https://corona.lmao.ninja/countries?sort=country"))


(def countries-data
  (reduce (fn [m each-country]
            (assoc m (:country each-country) {:country (:country each-country)
                                              :cases (:cases each-country)}))
          {}
          (-> covid19world-json
              (json/read-value (json/object-mapper {:decode-key-fn true})))))

(oz/view!
  (merge-with merge oz-config
              {:title    {:text "Confirmed COVID19 cases around the world"}
               :data     {:values (->> countries-data
                                       vals
                                       (map #(select-keys % [:country :cases])))},
               :mark     {:type "bar" :color (:green applied-science-palette)}
               :encoding {:x {:field "cases", :type "quantitative"}
                          :y {:field "country", :type "ordinal" :sort "-x"}}}))
