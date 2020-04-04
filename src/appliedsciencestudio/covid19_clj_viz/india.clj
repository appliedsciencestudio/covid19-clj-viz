(ns appliedsciencestudio.covid19-clj-viz.india
  "Visualization of coronavirus situation in India.

  Contributed by Noor Afshan Fathima.
  Statewise Hospital Beds data in India aggregation was helped by Noor Khuthejatul Kubra and Faiz Ur Rahman"
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

(def state-hospital-beds
  "From https://en.wikipedia.org/wiki/List_of_states_and_union_territories_of_India_by_population
  with commas manually removed"
  (into {} (map (juxt :State :Beds)
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
                      ;; TODO find lower-resolution geoJSON to speed up loading
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

;;;; ===========================================================================
;; http://www.cbhidghs.nic.in/showfile.php?lid=1147
;; National Health Profile 2019
;; https://pib.gov.in/PressReleasePage.aspx?PRID=1539877

(def state-population-and-beds
  (mcsv/read-csv "resources/india.state-population.tsv" {:fields [:state :population :beds]}))

;; dual axis tick plot visualizing the population and hospital beds in each state of India.
(oz/view!
  (merge-with merge oz-config
              {:data {:values state-population-and-beds}
               :encoding {:x {:field "state"
                              :axis {:domain false
                                     :title "States"
                                     :ticks false
                                     :labelAngle 90
                                     :labelPadding 10}
                              :type "ordinal"}}
               :layer [{:mark {:stroke "#85C5A6"
                               :type "tick"
                               :opacity 0.9}
                        :encoding {:y {:aggregate "average"
                                       :field "population"
                                       :type "quantitative"
                                       :axis {:title "Population"
                                              :titleColor "#85C5A6"}}}}
                       {:mark {:stroke "#d32f2f"
                               :type "tick"}
                        :encoding {:y {:aggregate "average"
                                       :field "beds"
                                       :type "quantitative"
                                       :axis {:title "No. of hospital beds"
                                              :titleColor "#d32f2f"}}}}]
               :resolve {:scale {:y "independent"}}}))

;-------------------------------------------------------------------------------------------------

;;cloropleth: India map visualizing the hospital beds distribution per 1k population of each state

(def india-geojson-with-beds-data
  (update (json/read-value (java.io.File. "resources/public/public/data/india-all-states.geo.json")
                           (json/object-mapper {:decode-key-fn true}))
          :features
          (fn [features]
            (mapv (fn [feature]
                    (let [state (:NAME_1 (:properties feature))
                          beds (get state-hospital-beds state)]
                      (assoc feature
                        :State         state
                        :beds          beds
                        :beds-per-1k   (double (Math/ceil (/ beds
                                                             (/ (get state-population state)
                                                                1000)))))))
                  features))))

(comment
  (json/write-value (java.io.File. "resources/public/public/data/india-all-states-created-with-beds.geo.json")
                    india-geojson-with-beds-data)

  ;; for inspection without flooding my REPL, we ignore the many many coordinates:
  (map #(dissoc % :geometry) (:features india-geojson-with-beds-data)))

(oz/view!
  (merge-with merge oz-config india-dimensions
              {:title {:text "Current India hospital beds count per 1k population"}
               :data {:name "india"
                      :values india-geojson-with-beds-data
                      :format {:property "features"}},
               :mark {:type "geoshape" :stroke "white" :strokeWidth 1}
               :encoding {:color {:field "beds-per-1k",
                                  :type "quantitative"
                                  :scale {:field "cases-per-100k",
                                          :scale {:type "log"}
                                          :type "quantitative"}}
                          :tooltip [{:field "State" :type "nominal"}
                                    {:field "beds-per-1k" :type "quantitative"}]}
               :selection {:highlight {:on "mouseover" :type "single"}}}))

;------------------------------------------------------------------------------------------------------------------------------------------------------
;;Experimentation plots, may not give the best visalizations for the data set I am considering. Still putting them here if someone wants to take a look.

(def state-population-measure
  "From https://en.wikipedia.org/wiki/List_of_states_and_union_territories_of_India_by_population"
  (->> (mcsv/read-csv "resources/india.state-population.tsv" {:fields [:state :population :beds]})
       (map (fn [each]
              {:state (:state each)
               :measure "population"
               :count (double (/ (:population each) 100000))}))))

(def state-bed-measure
  (->> (mcsv/read-csv "resources/india.state-population.tsv" {:fields [:state :population :beds]})
       (map (fn [each]
              {:state (:state each)
               :measure "beds"
               :count (double (/ (:beds each) 100000))}))))
;; stacked bar chart
(oz/view!
  (merge-with merge oz-config
              {:data {:values (concat state-population-measure state-bed-measure)}
               :mark {:type "bar"}
               :encoding {:x {:aggregate "sum"
                              :field "count"
                              :type "quantitative"
                              :stack "nomalize"}
                          :y {:field "state"
                              :type "nominal"}}
               :color {:field "measure"
                       :type "nominal"
                       :scale {:range  ["#675193" "#ca8861"]}}
               :opacity {:value 0.7}}))

;; area plot

(oz/view!
  (merge-with merge oz-config
              {:data {:values state-population-and-beds}
               :width 600
               :height 300
               :mark "area"
               :encoding {:x {:field "state"
                              :axis {:domain false
                                     :title "States"
                                     :ticks false
                                     :labelAngle 90
                                     :labelPadding 4}
                              :type "ordinal"}
                          :y {:field "population"
                              :type "quantitative"
                              :axis {:title "Avg. Population" :titleColor "#85C5A6"}}
                          :y2 {:field "beds"}
                          :opacaity {:value 0.7}}}))

;; dual axis bar graph
(oz/view!
  (merge-with merge oz-config
              {:data {:values state-population-and-beds}
               :encoding {:x {:field "state"
                              :axis {:domain false
                                     :title "States"
                                     :ticks false
                                     :labelAngle 90
                                     :labelPadding 10}
                              :type "ordinal"}}
               :layer [{:mark {:stroke "black"
                               :type "bar"
                               :color "#d32f2f"
                               :opacity 0.9}
                        :encoding {:y {:aggregate "average"
                                       :field "population"
                                       :type "quantitative"
                                       :axis {:title "Population"
                                              :titleColor "#85C5A6"}}}}
                       {:mark {:stroke "red"
                               :type "bar"}
                        :encoding {:y {:aggregate "average"
                                       :field "beds"
                                       :type "quantitative"
                                       :axis {:title "No. of beds"
                                              :titleColor "#85A9C5"}}}}]
               :resolve {:scale {:y "independent"}}}))

;; dual-axis line plot

(oz/view!
  (merge-with merge oz-config
              {:data {:values state-population-and-beds}
               :encoding {:x {:field "state"
                              :axis {:domain false
                                     :title "States"
                                     :ticks false
                                     :labelAngle 90
                                     :labelPadding 10}
                              :type "ordinal"}}
               :layer [{:mark {:stroke "#85C5A6"
                               :type "line"
                               :interpolate "monotone"}
                        :encoding {:y {:aggregate "average"
                                       :field "population"
                                       :type "quantitative"
                                       :axis {:title "Population"
                                              :titleColor "#85C5A6"}}}}
                       {:mark {:stroke "#85A9C5"
                               :type "line"
                               :interpolate "monotone"}
                        :encoding {:y {:aggregate "average"
                                       :field "beds"
                                       :type "quantitative"
                                       :axis {:title "No. of beds"
                                              :titleColor "#85A9C5"}}}}]
               :resolve {:scale {:y "independent"}}}))