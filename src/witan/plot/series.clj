(ns witan.plot.series
  (:require [clojure2d.color :as color]))

(defn series-type
  "Determine if a series is actual or median/iqr/95 by looking at the
  map."
  [data-map]
  (cond
    (:median data-map) :projected
    (:actual data-map) :actual
    :else (throw (ex-info "Couldn't determine series type."
                          {:data data-map}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make seq plotting friendly
(defn plot-friendly-kv [[k v] series-key x-key actual-key]
  [{::series-key (k series-key)
    ::x-key (k x-key)}
   (assoc v ::actual (v actual-key))])

(defn plot-friendly-kv-xf
  ([series-key x-key actual-key]
   (map (fn [x]
          (plot-friendly-kv x series-key x-key actual-key))))
  ([{::keys [series-key x-key actual-key]}]
   (plot-friendly-kv-xf series-key x-key actual-key)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Collect data
;;
;; Assumptions for the data->series
;;
;; 1. The data is only for one series, which will either be actual or
;; projected. So the map passed in will be a map of the x-values as
;; keys and the y values passed in for actual. For the iqr/95/median there will be a map
(defn collect-data-actual [series-map x-key {:keys [actual]}]
  (assoc-in
   (or series-map {})
   [:actual x-key]
   actual))

(defn collect-data-projected [series-map x-key {:keys [low-95pc-bound q1 median q3 high-95pc-bound]}]
  (-> (or series-map {})
      (assoc-in [:low-95pc-bound x-key] low-95pc-bound)
      (assoc-in [:q1 x-key] q1)
      (assoc-in [:median x-key] median)
      (assoc-in [:q3 x-key] q3)
      (assoc-in [:high-95pc-bound x-key] high-95pc-bound)))

(defn collect-data
  [series-map x-key data-map]
  (case (series-type data-map)
    :actual (collect-data-actual series-map x-key data-map)
    :projected (collect-data-projected series-map x-key data-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turn collected data into series
(defn ci [hi-data low-data {:keys [color alpha] :as spec}]
  (try
    [:ci
     (vector
      (into []
            (sort-by first hi-data))
      (into []
            (sort-by first low-data)))
     {:color (color/color color (or alpha 50))}]
    (catch Exception e
      (throw (ex-info "Couldn't create ci" {:hi hi-data :lo low-data :spec spec} e)))))

(defn line [data {:keys [color stroke dash size shape]
                  :or {stroke 4
                       size 15
                       shape \o}
                  :as spec}]
  (try
    (let [line-spec {:color (color/color color) :point {:type shape :size size :stroke {:size stroke}} :stroke {:size stroke}}
          line-spec (if dash (assoc-in line-spec [:stroke :dash] dash) line-spec)]
      [:line (into []
                   (sort-by first data))
       line-spec])
    (catch Exception e
      (throw (ex-info "Couldn't create line" {:data data :spec spec} e)))))

(defn collected-data->actual-series [[k {:keys [actual]}] series-specs domain-map]
  ;; TODO: Use domain map to plug 0s
  [(line actual (series-specs k))])

(defn collected-data->projected-series [[k {:keys [low-95pc-bound q1 median q3 high-95pc-bound]}] series-specs _]
  ;; All values should be in the projection
  [(ci high-95pc-bound low-95pc-bound (assoc (series-specs k) :alpha 25))
   (ci q3 q1 (series-specs k))
   (line median (assoc (series-specs k) :dash [2.0]))])

(defn collected-data->series [collected-data series-specs domain-map]
  (try
    (case (series-type (val collected-data))
      :actual (collected-data->actual-series collected-data series-specs domain-map)
      :projected (collected-data->projected-series collected-data series-specs domain-map))
    (catch Exception e
      (throw (ex-info "Unable to create series."
                      {:collected-data collected-data
                       :series-specs series-specs
                       :domain-map domain-map}
                      e)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data -> series
;;
;; Assumptions for the rf:
;;
;; 1. I'm only getting data that I want to plot in the same chart.
;;
;; 2. The data is a mix of actual and projected data.
(defn line-ci-series-missing-is-0-rf-f
  "Takes a seq of series specs and returns a rf to process data into a
  series of line and ci data structures with legends. Missing values
  froma domain map will be done as 0.

  The rf oprates over a series of:
  {:series-key foo :x-val calendar-year} {:actual population}
  {:series-key foo :x-val calendar-year} {:low-95 :high-95 :iqr3 :iqr1 :median}

  and produces solid lines for actual and dashed lines and ribbon
  plots of 50 and 25 alpha for median/iqr/95"
  [{::keys [series-specs domain-map]}]
  (fn
    ([]
     {::series-specs series-specs
      ::domain-map   domain-map
      ::series       (sorted-map)
      ::legend-spec  (sorted-set)})
    ([{::keys [series legend-spec]}]
     {::legend-spec (into []
                          (map (fn [series-key]
                                 (let [spec (series-specs series-key)]
                                   (try
                                     [:shape (:label spec series-key)
                                      {:color  (:color spec)
                                       :shape  (:legend-shape spec)
                                       :size   15
                                       :stroke {:size 4.0}}]
                                     (catch Exception e
                                       (throw (ex-info "Couldn't create legend"
                                                       {:spec spec :series-key series-key}
                                                       e)))))
                                 ))
                          legend-spec)
      ::series      (into []
                          (mapcat (fn [s] (collected-data->series s series-specs domain-map)))
                          series)})
    ([acc [{::keys [series-key x-key]} v]]
     (-> acc
         (update-in [::series series-key] collect-data x-key v)
         (update ::legend-spec (fnil conj []) series-key)))))


(defn line-ci-series-missing-is-0 [{::keys [data] :as config}]
  (transduce
   (plot-friendly-kv-xf config)
   (line-ci-series-missing-is-0-rf-f config)
   data))

(comment

  (def medians-example
    {:low-95pc-bound  {2020 1
                       2021 2
                       2022 3}
     :q1              {2020 3
                       2021 3
                       2022 3}
     :median          {2020 5
                       2021 5
                       2022 5}
     :q3              {2020 7
                       2021 7
                       2022 7}
     :high-95pc-bound {2020 9
                       2021 9
                       2022 9}})

  (def actuals-example
    {:actual {2020 5
              2021 5
              2022 5}})
  )
