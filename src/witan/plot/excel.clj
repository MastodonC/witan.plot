(ns witan.plot.excel
  (:require [dk.ative.docjure.spreadsheet :as xl]
            [witan.plot.chart :as pc]
            [witan.plot.series :as ps])
  (:import javax.imageio.ImageIO
           java.awt.image.BufferedImage
           java.io.ByteArrayOutputStream
           org.apache.poi.ss.usermodel.Workbook
           org.apache.poi.xssf.usermodel.XSSFWorkbook))

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

(defn excel-tab-string [tab-name]
  (if (< 31 (count tab-name))
    (subs tab-name 0 31)
    tab-name))

(defn add-sheet! [wb {::keys    [tab-name header xl-header-keys xl-data-keys]
                      :or       {xl-data-keys [:min :low-95pc-bound :q1 :median :q3 :high-95pc-bound :max]}
                      ::ps/keys [data]
                      :as       chart-spec}]
  (try
    (let [row-formatter (format-row-f xl-header-keys xl-data-keys)
          sheet         (xl/add-sheet! wb (excel-tab-string tab-name))
          _             (xl/add-rows! sheet (into [header]
                                                  (map row-formatter)
                                                  (sort-by (fn [[k _v]] ((apply juxt xl-header-keys) k)) data)))
          img           (-> chart-spec pc/image-buffer ->byte-array)
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

;; Add a sheet for each chart spec with the data and the chart
(defn ->workbook [chart-specs]
  (let [wb (create-empty-workbook)]
    (run! #(add-sheet! wb %) chart-specs)
    wb))

(defn save-workbook [filename wb]
  (xl/save-workbook! filename wb)
  wb)
