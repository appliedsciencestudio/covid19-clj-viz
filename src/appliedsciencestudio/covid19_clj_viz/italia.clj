(ns appliedsciencestudio.covid19-clj-viz.italia
  "Visualization of coronavirus situation in Italy.

  Contributed by David Schmudde.

  Relies on https://github.com/pcm-dpc/COVID-19 to be cloned into the `resources` directory."
  (:require [appliedsciencestudio.covid19-clj-viz.common :refer [oz-config]]
            [jsonista.core :as json]
            [meta-csv.core :as mcsv]
            [oz.core :as oz]))

;;;;;;;;;;;;;;;;;;;;
;; Conform Functions

(def fields-it->en
  "Mapping of CSV header names from Italian to their English translation."
  {;; For provinces (and some for regions too)
   "data"                    :date
   "stato"                   :state
   "codice_regione"          :region-code
   "denominazione_regione"   :region-name
   "codice_provincia"        :province-code
   "denominazione_provincia" :province-name
   "sigla_provincia"         :province-abbreviation
   "lat"                     :lat
   "long"                    :lon
   "totale_casi"             :cases
   ;; For regions
   "ricoverati_con_sintomi"      :hospitalized
   "terapia_intensiva"           :icu
   "totale_ospedalizzati"        :tot-hospitalized
   "isolamento_domiciliare"      :quarantined
   "totale_attualmente_positivi" :tot-positives
   "nuovi_attualmente_positivi"  :new-positives
   "dimessi_guariti"             :recovered
   "deceduti"                    :dead
   "tamponi"                     :tests})

(defn normalize-province-names
  "These region names a different on the population data files, geo.json files, and the covid-19 files."
  [province]
  (get
   {"Aosta" "Valle d'Aosta/Vallée d'Aoste"
    "Massa Carrara" "Massa-Carrara"
    "Bolzano" "Bolzano/Bozen"} province))

(defn normalize-region-names
  "These region names a different on the population data files, geo.json files, and the covid-19 files."
  [region]
  (get
   {"Friuli Venezia Giulia" "Friuli-Venezia Giulia"
    "Emilia Romagna" "Emilia-Romagna"
    "Valle d'Aosta" "Valle d'Aosta/Vallée d'Aoste"} region))

;; TODO: This is a nasty, unexpected edge case that would require a refactor to handle correctly
(defn normalize-trentino
  "REGION: The COVID numbers from Italy split one region into two. 'P.A. Bolzano' and 'P.A. Trento' should be combined into 'Trentino-Alto Adige/Südtirol.'"
  [region-covid-data]
  (let [keys-to-sum [:hospitalized :icu :tot-hospitalized :quarantined :tot-positives :new-positives :recovered :dead :cases :tests]
        regions-to-combine (filter #(or (= "P.A. Bolzano" (:region-name %))
                                        (= "P.A. Trento" (:region-name %)))
                                   region-covid-data)
    (->> (map #(select-keys % keys-to-sum) regions-to-combine) ; grab the important keys from the regions we want to combine
         (reduce #(merge-with + %1 %2))                        ; add the info from each key together
         (conj (first regions-to-combine))                     ; add in the rest of the information (name, region number, etc...)
         ((fn name-combined-region [combined-regions] (assoc combined-regions :region-name "Trentino-Alto Adige/Südtirol")))
         ((fn remove-inexact-data [combined-regions] (dissoc combined-regions :lat :lon))) ; lat/lon is no longer correct data
         (conj region-covid-data))))                           ; add this back into the main collection

(defn conform-to-territory-name
  "Index each map of territory information by territory name."
  [territories territory-key]
  (into {} (map #(vector (territory-key %) %) territories)))

(defn compute-cases-per-100k
  "If the data provided includes a valid population number, calculate the number for :cases-per-100k. Else set :cases-per-100k to nil.
   By retruning a non-number, any calculations or plotting will error or crash rather than work with bad data."
  [province-data-with-pop]
  (map #(let [cases (% :cases)
              population (% :population)
              calc-cases (fn [x] (double (/ cases x)))
              per-100k (fn [x] (/ x 100000))]
          (->> (if population
                 ((comp calc-cases per-100k) population)
                 nil)
               (assoc % :cases-per-100k))) province-data-with-pop))

;;;;;;;;;;;;;
;; COVID data

;; Now we can just read the CSV.
(def province-covid-data
  (->> (mcsv/read-csv "resources/Italia-COVID-19/dati-province/dpc-covid19-ita-province-latest.csv"
                      {:field-names-fn fields-it->en})
       (map #(update % :province-name (fn [territory-name]
                                        (if-let [update-territory-name (normalize-province-names territory-name)]
                                          update-territory-name
                                          territory-name))))))

(def region-covid-data
  (->> (mcsv/read-csv "resources/Italia-COVID-19/dati-regioni/dpc-covid19-ita-regioni-latest.csv"
                      {:field-names-fn fields-it->en})
       (map #(update % :region-name (fn [region-name]
                                      (if-let [update-region-name (normalize-region-names region-name)]
                                        update-region-name
                                        region-name))))
       (normalize-trentino)))

;;;;;;;;;;;;;;;;;;
;; Population Data

(def region-population-data
  "From http://www.comuni-italiani.it/province.html with updates to Trentino-Alto Adige/Südtirol and Valle d'Aosta/Vallée d'Aoste."
  (-> (mcsv/read-csv "resources/italy.region-population.csv" {:fields [:region-name :population :number-of-provinces]})
      (conform-to-territory-name :region-name)))

(def province-population-data
  "From http://www.comuni-italiani.it/province.html. Italy changed how provinces are structured in Sardina in 2016.
   Some are manually updated using the data here: https://en.wikipedia.org/wiki/Provinces_of_Italy"
  (-> (mcsv/read-csv "resources/italy.province-population.csv" {:fields [:province-name :population :abbreviation]})
      (conform-to-territory-name :province-name)))

(defn add-population-to-territories [all-territory-data all-territory-population territory-key]
  (map #(let [territory-to-update (% territory-key)]
          (->> (all-territory-population territory-to-update)
               (:population)
               (assoc % :population)))
       all-territory-data))

;;;;;;;;;;;;;
;; Final Data

(def region-data "For use with resources/public/public/data/limits_IT_regions-original.geo.json"
  (-> (add-population-to-territories region-covid-data region-population-data :region-name)
      (compute-cases-per-100k)
      (conform-to-territory-name :region-name)
      (dissoc "P.A. Bolzano" "P.A. Trento")))

(def province-data
  "For use with resources/public/public/data/limits_IT_provinces-original.geo.json"
  (-> (remove (comp #{"In fase di definizione/aggiornamento"} :province-name) province-covid-data)
      (add-population-to-territories province-population-data :province-name)
      (compute-cases-per-100k)
      (conform-to-territory-name :province-name)))

;;;; ===========================================================================
;;;; Coronavirus cases in Italy, by region and province

(def italia-region-geojson-with-data
  (update (json/read-value (java.io.File. "resources/public/public/data/limits_IT_regions-original.geo.json")
                           (json/object-mapper {:decode-key-fn true}))
          :features
          (fn [features]
            (mapv (fn [feature]
                    (assoc feature
                           :reg_name     (:reg_name (:properties feature))
                           :Cases          (get-in region-data [(:reg_name (:properties feature)) :cases] 0)
                           :Cases-per-100k (get-in region-data [(:reg_name (:properties feature)) :cases-per-100k] 0)))
                  features))))

(def italy-dimensions
  {:width 550 :height 700})

;; Regionally, we can see the north is affected strongly
(oz/view!
 (merge-with merge oz-config italy-dimensions
             {:title {:text "COVID19 cases in Italy, by province, per 100k inhabitants"}
              :data {:name "italy"
                     :values italia-region-geojson-with-data
                     :format {:property "features"}}
              :mark {:type "geoshape" :stroke "white" :strokeWidth 1}
              :encoding {:color {:field "Cases-per-100k"
                                 :type "quantitative"
                                 :scale {:domain [0 (apply max (map :cases-per-100k (vals region-data)))]}}
                         :tooltip [{:field "reg_name" :type "nominal"}
                                   {:field "Cases" :type "quantitative"}]}
              :selection {:highlight {:on "mouseover" :type "single"}}}))

;; Looking province-by-province, we can see how geographically concentrated the crisis is:
(oz/view!
 (merge-with merge oz-config italy-dimensions
             {:title {:text "COVID19 cases in Italy, by province, per 100k inhabitants"}
              :data {:name "italy"
                     :values (update (json/read-value (java.io.File. "resources/public/public/data/limits_IT_provinces-original.geo.json")
                                                      (json/object-mapper {:decode-key-fn true}))
                                     :features
                                     (fn [features]
                                       (mapv (fn [feature]
                                               (assoc feature
                                                      :prov_name     (:prov_name (:properties feature))
                                                      :Cases          (get-in province-data [(:prov_name (:properties feature)) :cases] 0)
                                                      :Cases-per-100k (get-in province-data [(:prov_name (:properties feature)) :cases-per-100k] 0)))
                                             features)))
                     :format {:property "features"}}
              :mark {:type "geoshape" :stroke "white" :strokeWidth 1}
              :encoding {:color {:field "Cases-per-100k"
                                 :type "quantitative"
                                 :scale {:domain [0 (apply max (map :cases-per-100k (vals province-data)))]}}
                         :tooltip [{:field "prov_name" :type "nominal"}
                                   {:field "Cases-per-100k" :type "quantitative"}]}
              :selection {:highlight {:on "mouseover" :type "single"}}}))
