(ns appliedsciencestudio.covid19-clj-viz.india
  "Visualization of coronavirus situation in India.

  Contributed by Noor Afshan Fathima."
  (:require [appliedsciencestudio.covid19-clj-viz.common :refer [applied-science-font
                                                                 oz-config
                                                                 applied-science-palette]]
            [appliedsciencestudio.covid19-clj-viz.sources.johns-hopkins :as jh]
            [clojure.set :refer [rename-keys]]
            [clojure.string :as string]
            [jsonista.core :as json]
            [meta-csv.core :as mcsv]
            [oz.core :as oz]))

(comment
  (oz/start-server! 8082))



(defonce covid19india-json
  (slurp "https://api.covid19india.org/data.json"))

(def state-data
  "Coronavirus cases, by Indian state"
  (reduce (fn [m st]
            (assoc m (:state st)
                   {:confirmed (Integer/parseInt (:confirmed st))
                    :active    (Integer/parseInt (:active st))
                    :recovered (Integer/parseInt (:recovered st))
                    :deaths    (Integer/parseInt (:deaths st))
                    :state (:state st)}))
          {}
          (-> covid19india-json
              (json/read-value (json/object-mapper {:decode-key-fn true}))
              :statewise)))

(def state-population
  "From https://en.wikipedia.org/wiki/List_of_states_and_union_territories_of_India_by_population
  with commas manually removed"
  (into {} (map (juxt :State :Population)
                (mcsv/read-csv "resources/india.state-population.tsv"))))


;;;; ===========================================================================
;; Minimum viable geographic visualization of India
(oz/view! {:data {:url "/public/data/india-all-states.geo.json"
                  :format {:type "json" :property "features"}}
           :mark "geoshape"})

(def india-dimensions
  {:width 750 :height 750})

(def india-geojson-with-data
  (update (json/read-value (java.io.File. "resources/public/public/data/india-all-states.geo.json")
                           (json/object-mapper {:decode-key-fn true}))
          :features
          (fn [features]
            (mapv (fn [feature]
                    (let [state (:NAME_1 (:properties feature))
                          cases (get-in state-data [state :confirmed] 0)]
                      (assoc feature
                        :State          state
                        :Cases          cases
                        :Deaths         (get-in state-data [state :deaths] 0)
                        :Recovered      (get-in state-data [state :recovered] 0)
                        :Cases-per-100k (double (/ cases
                                                   (/ (get state-population state)
                                                      100000))))))
                  features))))

(comment
  (json/write-value (java.io.File. "resources/public/public/data/india-all-states-created.geo.json")
                    india-geojson-with-data)

  ;; for inspection without flooding my REPL, we ignore the many many coordinates:
  (map #(dissoc % :geometry) (:features india-geojson-with-data)))
  



;;;; ===========================================================================
;;;; Choropleth of Coronavirus situation in India
;;;; (It may take a while to load; I think because the geoJSON is very detailed)
(oz/view!
  (merge-with merge oz-config india-dimensions
              {:title {:text "Current India COVID-19 Scenario"}
               :data {:name "india"
                      :values india-geojson-with-data
                      :format {:property "features"}},
               :mark {:type "geoshape" :stroke "white" :strokeWidth 1}
               :encoding {:color {:field "Cases-per-100k",
                                  :type "quantitative"
                                  :scale {:field "cases-per-100k",
                                          :scale {:type "log"}
                                          :type "quantitative"}}
                          :tooltip [{:field "State" :type "nominal"}
                                    {:field "Cases" :type "quantitative"}
                                    {:field "Deaths" :type "quantitative"}
                                    {:field "Recovered" :type "quantitative"}]}
               :selection {:highlight {:on "mouseover" :type "single"}}}))


;;;; ===========================================================================
;;;; Bar chart with Indian states
(oz/view!
 (merge-with merge oz-config
             {:title {:text "Confirmed COVID19 cases in India"}
              :data {:values (->> state-data
                                  vals
                                  (map #(select-keys % [:state :confirmed]))
                                  ;; ;; FIXME this is the line to toggle:
                                  (remove (comp #{"Total"} :state)))},
              :mark {:type "bar" :color (:green applied-science-palette)}
              :encoding {:x {:field "confirmed", :type "quantitative"}
                         :y {:field "state", :type "ordinal" :sort "-x"}}}))


;;;; ===========================================================================
;;;; Bar chart with Indian states and Chinese provinces
(oz/view!
  (merge oz-config
         {:title "Confirmed COVID19 cases in China and India",
          :data {:values (let [date "2020-03-19"]
                           (->> jh/confirmed
                                (map #(select-keys % [:province-state :country-region date]))
                                (filter (comp #{"China" "Mainland China"} :country-region))
                                (map #(rename-keys % {date :confirmed}))
                                (concat (->> state-data
                                             vals
                                             (map (comp #(assoc % :country-region "India")
                                                        #(rename-keys % {:state :province-state})))
                                             (sort-by :state)))
                                (remove (comp #{"Hubei" "Total"} :province-state))))}
          :mark "bar"
          :encoding {:x {:field "confirmed", :type "quantitative"}
                     :y {:field "province-state", :type "ordinal" :sort "-x"}
                     :color {:field "country-region" :type "ordinal"
                             :scale {:range [(:purple applied-science-palette)
                                             (:green applied-science-palette)]}}}}))

;;;; ===========================================================================
;;;; Daily new cases in a particular country over the past N days
(oz/view!
  (merge-with merge oz-config
              {:title {:text "Daily new confirmed COVID-19 cases"
                       :font (:mono applied-science-font)
                       :fontSize 30
                       :anchor "middle"}
               :width 500 :height 325
               :data {:values (let [country "India"]
                                (->> (jh/new-daily-cases-in :confirmed country)
                                     (sort-by key #(compare %2 %1))
                                     (take 20)
                                     vals
                                     (into [])
                                     (map-indexed (fn [i n] {:cases n
                                                             :country country
                                                             :days-ago i}))))},
               :mark {:type "bar" :size 24}
               :encoding {:x {:field "days-ago" :type "ordinal"
                              :sort "descending"}
                          :y {:field "cases", :type "quantitative"}
                          :tooltip {:field "cases" :type "quantitative"}
                          :color {:field "country" :type "nominal"
                                  :scale {:range (mapv val applied-science-palette)}}}}))
