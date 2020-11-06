(ns witan.plot.excel
  (:require [dk.ative.docjure.spreadsheet :as xl]
            [witan.plot.series :as ps]
            [witan.plot.chart :as pc]
            [witan.plot.chart :as chart])
  (:import javax.imageio.ImageIO
           java.awt.image.BufferedImage
           java.io.ByteArrayOutputStream
           org.apache.poi.ss.usermodel.Workbook
           org.apache.poi.xssf.usermodel.XSSFWorkbook))

#_(defn comparison-table
    "Turn comparison definition data into a vector of vectors for csv or excel output"
    [{:keys [series domain-key y-axis-label x-axis-label] :as comparison-defs}]
    (into [["label"
            (name domain-key)
            x-axis-label
            y-axis-label
            "min" "low 95pc bound" "q1" "median" "q3" "high 95pc bound" "max" "iqr"]]
          (comp
           (map (fn [{:keys [legend-label projection-data historical-data]}]
                  {:historical-data (into [] (map (fn [record] (assoc record :label legend-label))) historical-data)
                   :projection-data (into [] (map (fn [record] (assoc record :label legend-label))) projection-data)}))
           (mapcat (juxt :historical-data :projection-data))
           cat
           (map (juxt :label
                      domain-key
                      :calendar-year
                      :population
                      :min :low-95pc-bound :q1 :median :q3 :high-95pc-bound :max :iqr)))
          series))


#_(defn domain-charts [{:keys [domain-key chart-base-def serie-base-def colors-and-points historical-data projection-data]} titles-and-sets]
    (into []
          (comp
           (map (fn [[title domain-values]]
                  (assoc
                   chart-base-def
                   :title title
                   :series
                   (into []
                         (comp
                          (map (fn [domain-value]
                                 (merge serie-base-def
                                        {:legend-label (get-in chart-base-def [:domain-values-lookup domain-value] domain-value)
                                         :color (-> domain-value colors-and-points :color)
                                         :shape (-> domain-value colors-and-points :point)
                                         :projection-data (into [] (filter #(= domain-value (domain-key %))) projection-data)
                                         :historical-data (into [] (filter #(= domain-value (domain-key %))) historical-data)})))
                          (remove #(and (empty? (:historical-data %)) (empty? (:projection-data %)))))
                         domain-values))))
           (remove #(empty? (:series %)))
           (map comparison-chart-and-table))
          titles-and-sets))

(defn ->byte-array [^BufferedImage image]
  (with-open [out (ByteArrayOutputStream.)]
    (ImageIO/write image "png" out)
    (.toByteArray out)))

(defn create-empty-workbook []
  (XSSFWorkbook.))

(defn format-row-f [xl-header-keys xl-data-keys]
  (fn [[k v]]
    (into
     ((apply juxt xl-header-keys) k)
     ((apply juxt xl-data-keys) v))))

(defn add-sheet! [wb {::keys    [tab-name header xl-header-keys xl-data-keys]
                      :or       {xl-data-keys [:min :low-95pc-bound :q1 :median :q3 :high-95pc-bound :max]}
                      ::ps/keys [data]
                      :as       chart-spec}]
  (try
    (let [row-formatter (format-row-f xl-header-keys xl-data-keys)
          sheet         (xl/add-sheet! wb tab-name)
          _             (xl/add-rows! sheet (into [header]
                                                  (map row-formatter)
                                                  (sort-by (fn [[k _v]] ((apply juxt xl-header-keys) k)) data)))
          img           (-> chart-spec chart/image-buffer ->byte-array)
          ;; ;; int pictureIdx = wb.addPicture(bytes, Workbook.PICTURE_TYPE_JPEG);
          pic-idx (.addPicture wb img Workbook/PICTURE_TYPE_PNG)
          helper (.getCreationHelper wb)
          drawing (.createDrawingPatriarch sheet)
          anchor (.createClientAnchor helper)
          _ (.setCol1 anchor 14)
          _ (.setRow1 anchor 2)
          ;; Picture pict = drawing.createPicture(anchor, pictureIdx);
          ;; pict.resize();
          pict (.createPicture drawing anchor pic-idx)
          _ (.resize pict)]
      sheet)
    (catch Exception e
      (throw (ex-info (str "Failed to create sheet. " tab-name) {:chart-spec chart-spec} e)))))

#_(let [wb-data (into []
                      (mapcat (fn [cd]
                                [(:title cd)
                                 (:table cd)]))
                      chart-specs)
        wb-charts (into []
                        (map (fn [cd]
                               [(:title cd)
                                (-> cd :chart :chart-image :buffer ->byte-array)]))
                        chart-specs)
        wb (apply xl/create-workbook wb-data)]
    (run! (fn [[sheet-name img]]
            (try
              (let [ ;; int pictureIdx = wb.addPicture(bytes, Workbook.PICTURE_TYPE_JPEG);
                    pic-idx (.addPicture wb img Workbook/PICTURE_TYPE_PNG)
                    sheet (xl/select-sheet sheet-name wb)
                    helper (.getCreationHelper wb)
                    drawing (.createDrawingPatriarch sheet)
                    anchor (.createClientAnchor helper)
                    _ (.setCol1 anchor 14)
                    _ (.setRow1 anchor 2)
                    ;; Picture pict = drawing.createPicture(anchor, pictureIdx);
                    ;; pict.resize();
                    pict (.createPicture drawing anchor pic-idx)]
                (.resize pict))
              (catch Exception e
                (throw (ex-info (str "Failed to create sheet. " sheet-name) {:sheet-name sheet-name} e)))))
          wb-charts)
    wb)

;; Add a sheet for each chart spec with the data and the chart
(defn ->workbook [chart-specs]
  (let [wb (create-empty-workbook)]
    (run! #(add-sheet! wb %) chart-specs)
    wb))


(defn save-workbook [filename wb]
  (xl/save-workbook! filename wb))
