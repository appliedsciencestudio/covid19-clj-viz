(ns appliedsciencestudio.covid19-clj-viz.south-america
  "Visualization of coronavirus situation in South America.
  
  Contributed by Yuliana Apaza and Paula Asto."
  (:require [appliedsciencestudio.covid19-clj-viz.sources.johns-hopkins :as jh]
            [appliedsciencestudio.covid19-clj-viz.sources.world-bank :as wb]
            [appliedsciencestudio.covid19-clj-viz.common :refer [oz-config
                                                                 applied-science-palette]]
            [meta-csv.core :as mcsv]
            [clojure.set :refer [rename-keys]]
            [jsonista.core :as json]
            [oz.core :as oz]))

(def southamerica-cases
  "Current number of COVID19 cases in South America, by countries"
  (let [date "2020-03-28"]
    (->> jh/confirmed
         (map #(select-keys % [:country-region date]))
         (filter (comp #{"Peru" "Brazil" "Argentina"
                         "Chile" "Bolivia" "Colombia"
                         "Ecuador" "Uruguay" "Paraguay" "Venezuela"
                         "Suriname" "Guyana"}
                       :country-region))
         (reduce (fn [m row]
                   (assoc m (:country-region row) (get row date 0)))
                 {}))))

(defn normalize-peru-regions [r]
  (get {"AMAZONAS" "Amazonas"
        "ANCASH" "Ancash"
        "APURIMAC" "Apurímac"
        "AREQUIPA" "Arequipa"
        "AYACUCHO" "Ayacucho"
        "CAJAMARCA" "Cajamarca"
        "CALLAO" "Callao"
        "CUSCO" "Cusco"
        "HUANCAVELICA" "Huancavelica"
        "HUANUCO" "Huánuco"
        "ICA" "Ica"
        "JUNIN" "Junín"
        "LA LIBERTAD" "La Libertad"
        "LAMBAYEQUE" "Lambayeque"
        "LIMA" "Lima"
        "LORETO" "Loreto"
        "MADRE DE DIOS" "Madre de Dios"
        "MOQUEGUA" "Moquegua"
        "PASCO" "Pasco"
        "PIURA" "Piura"
        "PUNO" "Puno"
        "SAN MARTIN" "San Martín"
        "TACNA" "Tacna"
        "TUMBES" "Tumbes"
        "UCAYALI" "Ucayali"}
       r r))

(def peru-cases
  "Mapping from Peruvian region to number of cases (as of 28 March 2020)."
  (reduce (fn [m {:keys [region cases]}]
            (assoc m (normalize-peru-regions region) cases))
          {}
          (mcsv/read-csv "resources/peru.covid19cases-march28.csv"
                         {:fields [:region :cases]})))

(def peru-region-populations
  "Population of regions of Peru.
  From http://statoids.com/upe.html which cites the 2007-10-21 census."
  (reduce (fn [m row]
            (assoc m (:region row) (:population row)))
          {}
          (mcsv/read-csv "resources/peru-region-populations.tsv"
                         {:fields [:region
                                   :hasc :iso :fips :nute :inei
                                   :population :area-km2 :area-mile2
                                   :capital]})))

(def south-america-geojson-with-data
  (update (json/read-value (java.io.File. "resources/public/public/data/southamerica.geo.json")
                           (json/object-mapper {:decode-key-fn true}))
          :features
          (fn [features]
            (mapv (fn [feature]
                    (let [country (:geounit (:properties feature))
                          cases (get southamerica-cases country)]
                      (assoc feature
                             :country country
                             :cases cases
                             :cases-per-100k
                             (if-let [pop (get wb/country-populations country)]
                               (double (/ cases (/ pop 100000)))
                               0))))
                  features))))

(comment ;;;; Check populations (nonce code)
  ;; Which countries are we looking at?
  (keys southamerica-cases)
  
  (map :country (:features south-america-geojson-with-data))

  ;; Which country do we not have population data for?
  (map (juxt identity #(get wb/country-populations %))
       (map :country (:features south-america-geojson-with-data)))


  (map (juxt identity #(get peru-region-populations %))
       (map :region (:features peru-geojson-with-data)))

  
  )

(def peru-geojson-with-data
  (update (json/read-value (java.io.File. "resources/public/public/data/peru-regions.geo.json")
                           (json/object-mapper {:decode-key-fn true}))
          :features
          (fn [features]
            (mapv (fn [feature]
                    (let [region (normalize-peru-regions (:NOMBDEP (:properties feature)))
                          cases (get peru-cases region 0)]
                      (assoc feature
                             :region region
                             :cases (get peru-cases region cases)
                             :cases-per-100k
                             (if-let [pop (get peru-region-populations
                                               (normalize-peru-regions region) 0)]
                               (double (/ cases (/ pop 100000)))
                               0))))
                  features))))

(comment
  (oz/start-server! 8082)

  )

(def map-dimensions
  {:width 550 :height 700})


;;;; ===========================================================================
;;;; COVID-19 cases in South America, by country, scaled to population
(oz/view!
 (merge-with merge oz-config map-dimensions
             {:title {:text "COVID-19 cases in South America by country"}
              :data {:name "south-america"
                     :values south-america-geojson-with-data
                     :format {:property "features"}}
              :mark {:type "geoshape" :stroke "white" :strokeWidth 1}
              :encoding {:color {:field "cases-per-100k" :type "quantitative"
                                 :condition {:test  "datum['cases-per-100k'] == 0"
                                             :value (:gray applied-science-palette)}}
                         :tooltip [{:field "country" :type "nominal"}
                                   {:field "cases" :type "quantitative"}
                                   {:field "cases-per-100k" :type "quantitative"}]}
              :selection {:highlight {:on "mouseover" :type "single"}}}))


;;;; ===========================================================================
;;;; COVID-19 cases in Peru, by region, scaled to population
(oz/view!
 (merge-with merge oz-config map-dimensions
             {:title {:text "COVID-19 cases in Peru by Regions"}
              :data {:name "peru"
                     :values peru-geojson-with-data
                     :format {:property "features"}}
              :mark {:type "geoshape" :stroke "white" :strokeWidth 1}
              :encoding {:color {:field "cases-per-100k"
                                 :type "quantitative"
                                 :condition {:test  "datum['cases-per-100k'] == 0"
                                             :value (:gray applied-science-palette)}}
                         :tooltip [{:field "region" :type "nominal"}
                                   {:field "cases" :type  "quantitative"}
                                   {:field "cases-per-100k" :type "quantitative"}]}
              :selection {:highlight {:on "mouseover" :type "single"}}}))
