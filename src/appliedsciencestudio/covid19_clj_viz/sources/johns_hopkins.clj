(ns appliedsciencestudio.covid19-clj-viz.sources.johns-hopkins
  "Johns Hopkins COVID19 data sources, with util fns.

  Several vars rely on the Johns Hopkins repo to be cloned into the
  `resources` directory. See README."
  (:require [appliedsciencestudio.covid19-clj-viz.sources.world-bank :as world-bank]
            [meta-csv.core :as mcsv]
            [clojure.string :as string])
  (:import [java.time LocalDate]
           [java.time.format DateTimeFormatter]))

(defn parse-covid19-date [mm-dd-yy]
  (LocalDate/parse mm-dd-yy (DateTimeFormatter/ofPattern "M/d/yy")))

(defn field-names [^String s]
  (if (#{"Province/State" "Country/Region" "Lat" "Long"} s)
    (-> s
        string/lower-case
        (string/replace "/" "-")
        keyword)
    (str (parse-covid19-date s))))

(def confirmed
  "From https://github.com/CSSEGISandData/COVID-19/tree/master/who_covid_19_situation_reports
   Existence of this file relies on cloning Johns Hopkins repo into resources directory. See README."
  (mcsv/read-csv "resources/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
                 {:field-names-fn field-names}))

(def recovered
  "From https://github.com/CSSEGISandData/COVID-19/tree/master/who_covid_19_situation_reports
   Existence of this file relies on cloning Johns Hopkins repo into resources directory. See README."
  (mcsv/read-csv "resources/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
                 {:field-names-fn field-names}))

(def deaths
  "From https://github.com/CSSEGISandData/COVID-19/tree/master/who_covid_19_situation_reports
   Existence of this file relies on cloning Johns Hopkins repo into resources directory. See README."
  (mcsv/read-csv "resources/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
                 {:field-names-fn field-names}))

(def csv-dates
  (drop 4 (map first (first confirmed))))

(def last-reported-date
  (key (last (first confirmed))))

(def countries
  (set (map :country-region confirmed)))

(defn data-exists-for-country? [kind country]
  (let [rows (case kind
              :recovered recovered
              :deaths    deaths
              :confirmed confirmed)]
   (some (comp #{country} :country-region) rows)))

(defn country-totals
  "Aggregates given data for `country`, possibly spread across
  provinces/states, into a single sequence ('row')."
  [country rows]
  (->> rows
       (filter (comp #{country} :country-region))
       (map #(dissoc % :country-region :province-state :lat :long))
       (map vals)
       (apply map +)))

(defn new-daily-cases-in [kind country]
  (assert (#{:recovered :deaths :confirmed} kind))
  (assert (data-exists-for-country? kind country))
  (let [rows (case kind
               :recovered recovered
               :deaths    deaths
               :confirmed confirmed)]
    (->> rows
         (country-totals country)
         (partition 2 1)
         (reduce (fn [acc [n-yesterday n-today]]
                   (conj acc (max 0 (- n-today n-yesterday))))
                 [])
         (zipmap csv-dates)
         (sort-by key))))

(comment
  (country-totals "Germany" confirmed)
  (country-totals "India" confirmed)
  (country-totals "Australia" deaths)
  
  (country-totals "Australia" confirmed)
  (country-totals "Australia" deaths)
  
  (new-daily-cases-in :confirmed "Germany")
  (new-daily-cases-in :confirmed "India")
  
  (new-daily-cases-in :confirmed "Korea, South")

  ;; test a country with province-level data
  (new-daily-cases-in :confirmed "Australia"))
  


(defn rate-as-of
  "Day-to-day COVID-19 case increase."
  [kind country days]
  (assert (#{:recovered :deaths :confirmed} kind))
  (let [country (get world-bank/normalize-country
                     country country)]
    (if (data-exists-for-country? kind country)
      (let [rows (case kind
                   :recovered recovered
                   :deaths    deaths
                   :confirmed confirmed)
            row (partition 2 1 (country-totals country rows))
            [a b] (nth row (- (count row) (inc days)))]
        (if (zero? a)
          0
          (double (/ b a))))
      0)))

(comment

  (country-totals "Germany" deaths)
  ;; (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
  ;; 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 2 3 3 7 9 11 17 24)

  (country-totals "Germany" confirmed)
  ;; (0 0 0 0 0 1 4 4 4 5 8 10 12 12 12 12 13 13 14 14 16 16 16 16 16
  ;; 16 16 16 16 16 16 16 16 16 17 27 46 48 79 130 159 196 262 482 670
  ;; 799 1040 1176 1457 1908 2078 3675 4585 5795 7272 9257)  
  
  (map (partial rate-as-of :confirmed "Germany")
       [1 2 3 5 10 20 30])
  ;; (1.272964796479648 1.254874892148404 1.263904034896401 1.768527430221367 1.30162703379224 1.703703703703704 1.0)

  (map (partial rate-as-of :confirmed "India")
       [1 2 3 5 10 20 30]))
