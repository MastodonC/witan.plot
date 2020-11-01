(ns witan.plot.chart
  (:require [cljplot.build :as plotb]
            [cljplot.render :as plotr]
            [witan.plot.colors :as colors]
            [witan.plot.series :as ps]
            [cljplot.core :as plot]))

(defn zero-index-numerical-y-axes [prepped-data]
  (let [[t [_bottom top]] (get-in prepped-data [:extents :y 0])]
    (if (= :numerical t)
      (plotb/update-scale prepped-data :y :domain [0 (+ (* 0.025 top) top)])
      prepped-data)))

(defn update-chart-y-axis-ticks [chart-spec series]
  (let [[t [_bottom top]] (get-in chart-spec [:extents :y 0])]
    (if (and (= :numerical t) (< top 10))
      (plotb/update-scale chart-spec :y :domain [0 10])
      chart-spec)))

(defn update-chart-x-axis-ticks [chart-spec series]
  (let [x-count (->> series
                     (mapcat (fn [x] (second x)))
                     (map (fn [y] (first y)))
                     (into #{})
                     count)]
    (if (< x-count 10)
      (plotb/update-scale chart-spec :x :ticks x-count)
      chart-spec)))

(defn zero-y-index [{::ps/keys [series legend-spec]
                     ::keys [x-axis
                             y-axis
                             size
                             legend-label ;; new
                             title]
                     :as chart-spec}]
  (let [size (or size {:width 1539 :height 1037 :background colors/white}) ;; 1539x1037 is almost exactly the right size to go into the slide
        title-format (or (:format title) {:font-size 36 :font "Open Sans Bold" :font-style :bold :margin 36})]
    (-> (into [[:grid]] series) ;; this is good
        (plotb/preprocess-series)
        (plotb/update-scale :x :fmt (::tick-formatter x-axis))
        (update-chart-x-axis-ticks series)
        (plotb/update-scale :y :fmt (::tick-formatter y-axis))
        (zero-index-numerical-y-axes)
        (update-chart-y-axis-ticks series)
        (plotb/add-axes :bottom {:ticks {:font-size 24 :font-style nil}})
        (plotb/add-axes :left {:ticks {:font-size 24 :font-style nil}})
        (plotb/add-label :bottom (::label x-axis) {:font-size 36 :font "Open Sans" :font-style nil})
        (plotb/add-label :left (::label y-axis) {:font-size 36 :font "Open Sans" :font-style nil})
        (plotb/add-label :top (::label title) title-format)
        (plotb/add-legend legend-label legend-spec)
        (plotr/render-lattice size))))

(defn show [plot]
  (plot/show plot))

(comment

  (defn free-y [{:keys [x-axis y-axis legend title series size]}]
    (let [size (or size {:width 1024 :height 768 :background colors/white})
          title-format (or (:format title) {:font-size 24 :font "Open Sans Bold" :margin 36})]
      (-> (apply plotb/series series)
          (plotb/preprocess-series)
          (plotb/update-scale :x :fmt (:tick-formatter x-axis))
          ;; (plotb/update-scale :x :ticks 5) ;; only needed if we have fewer than 10 points so we don't get double on the axis?
          (plotb/update-scale :y :fmt (:tick-formatter y-axis))
          (plotb/add-axes :bottom {:ticks {:font-size 16}})
          (plotb/add-axes :left {:ticks {:font-size 16}})
          (plotb/add-label :bottom (:label x-axis) {:font-size 20 :font "Open Sans" :font-style nil})
          (plotb/add-label :left (:label y-axis) {:font-size 20 :font "Open Sans" :font-style nil})
          (plotb/add-label :top (:label title) title-format)
          (plotb/add-legend (:label legend) (:legend-spec legend))
          (plotr/render-lattice size))))

  )
