(ns appliedsciencestudio.covid19-clj-viz.sources.world-bank
  (:require [meta-csv.core :as mcsv]))

(def normalize-country
  "Mappings to normalize naming & typographic variation to a single standard."
  {"UK" "United Kingdom"
   "The former Yugoslav Republic of Macedonia" "North Macedonia"
   "Republic of Moldova" "Moldova"
   ;; "Faroe Islands"
   ;; "Holy See (Vatican City)"
   "Czech Republic" "Czechia"
   "Venezuela, RB" "Venezuela"})

(def country-populations
  "From https://data.worldbank.org/indicator/SP.POP.TOTL
  NB: this dataset has some odd or crufty rows"
  (->> (mcsv/read-csv "resources/API_SP.POP.TOTL_DS2_en_csv_v2_821007.csv"
                      {:skip 5 :fields (concat [:country-name :country-code nil nil]
                                               (map (fn [n] {:field (str n)
                                                            :type :long})
                                                    (range 1960 2020))
                                               [nil])})
       (remove (comp #{"Not classified"} :country-name))
       (reduce (fn [m {:keys [country-name] :as row}]
                 (assoc m (get normalize-country country-name country-name)
                        ;; use the most recent non-null population value
                        (some #(get row (str %)) (range 2019 1959 -1))))
               {})))
