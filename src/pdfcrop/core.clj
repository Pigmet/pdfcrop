(ns pdfcrop.core
  (:gen-class)
  (:require [texdata.core :refer [tex]]
            [swinghelp.core :refer [sset! sset-class! sget]]
            [clojure.string :refer [join]]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh with-sh-dir]]
            [pdfcrop.helpers :refer [swap-when! file-data]])
  (:use [seesaw core font color graphics])
  (:import
   (java.io File)
   (javax.swing JFileChooser)
   (javax.swing.filechooser FileFilter)
   (javax.swing.filechooser FileNameExtensionFilter)
   (java.awt.image BufferedImage)
   (java.awt Canvas)
   (org.apache.pdfbox.pdmodel PDDocument PDPage)
   (org.apache.pdfbox.rendering PDFRenderer ImageType)
   (org.apache.pdfbox.pdmodel.common PDRectangle)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (run))

;; TODO : fix bugs (the software throws exception while in use)

(defn- crop-pdf-impl
  [src dest page-num [x1 y1] [x2 y2]]
  (with-open [doc (PDDocument/load (io/file src))]
    (let [page (.getPage doc page-num)]
      (.setCropBox page (new PDRectangle x1 y1 (- x2 x1) (- y2 y1)))
      (.save doc (io/file dest)))))

(defn- pdf-file-data [f]
  (with-open [doc (PDDocument/load (io/file f))]
    {:page-number (.getNumberOfPages doc )}))

(defn- get-pdf-page [f n]
  (with-open [doc (PDDocument/load (io/file f))]
    (.getPage doc n)))

(defn get-crop-box [page]
  (.getCropBox page))

(defn- cropbox-data
  ([path] (cropbox-data path 0))
  ([path n]
   (let [box (-> path (get-pdf-page n) get-crop-box)]
     {:width (.getWidth box)
      :height (.getHeight box)})))

(defn- rectangle-data [^PDRectangle x]
  {:height (.getHeight x)
   :width (.getWidth x)
   :lower-left-x (.getLowerLeftX x)
   :lower-left-y (.getLowerLeftY x)
   :upper-right-x (.getUpperRightX x)
   :upper-right-y (.getUpperRightY x)})

(defn convert-to-image
  "Takes filepath and page number and returns BufferedImage."
  ([src n] (convert-to-image src n 300))
  ([src n dpi]
   (with-open [doc (PDDocument/load (io/file src))]
     (let [renderer (new PDFRenderer doc)]
       (.renderImageWithDPI renderer n dpi ImageType/RGB)))))

(defn- resize-image [im new-width new-height]
  (let [temp (.getScaledInstance im new-width new-height BufferedImage/SCALE_SMOOTH)
        ret (buffered-image new-width new-height)
        g (.createGraphics ret)]
    (.drawImage g temp 0 0 nil)
    ret))

(defn bimage-data [^BufferedImage x]
  {:height (.getHeight x)
   :width (.getWidth x)})

(defn- crop-pdf-ratio-coll-page
  "Start and end are vectors signifying the cropping area in percentage."
  [src dest from-page to-page start end]  
  (let [{w :width h :height } (cropbox-data src)
        invert (fn [[x y]] [x ( - 1 y)])
        [start end] (map invert [start end])
        start (map * start [w h])
        end (map * end [ w h])
        new-rect (fn [x y width height] (new PDRectangle x y width height))
        rect (apply new-rect (concat start (map - end start)))]
    (with-open [doc (PDDocument/load (io/file src))]
      (dorun
       (map
        #(.setCropBox (.getPage doc %) rect)
        (range from-page to-page)))
      (.save doc (io/file dest)))))

(defn- crop-pdf-ratio-all-page [src dest start end]
  (let [n (:page-number (pdf-file-data src))]
    (crop-pdf-ratio-coll-page src dest 0 n start end)))

(defn- new-file-filter [extension]
  (proxy [FileFilter] []
    (accept [file]
      (or (.isDirectory file)
          (-> file str (.endsWith (str "." extension)))))
    (getDescription [] (format "%s files" extension))))

;; state

(def plot-data {:height 500 :width 500})

(def state-init {:src nil
                 :dest :nil
                 :page 0
                 :start [0 0]
                 :end [0 0]})

(def state (atom state-init))

(defn- reset-state-id [& ids]
  (reset! state (merge @state (select-keys state-init ids))))

(defn- reset-state! [] (reset! state state-init))

(defn- state-ok? [{:keys [src page]}]
  (when src
    (let [{total-pages :page-number} (pdf-file-data src)]
      (<= 0 page (dec total-pages)))))

(defn- ui-part []
  (horizontal-panel
   :items [(button :text "load" :id :load :class :text)
           (menubar :items
                    [(menu :text "menu" :class :text
                           :items [(button :text "close" :id :close
                                           :class :text)])])]))

(defn- new-frame []
  (frame :width 500
         :height 500
         :title "crop pdf"
         :content (border-panel
                   :north (ui-part)
                   :center (canvas :id :canvas))))

;; load

(defn load-file []
  ())

(defn- set-font [root f]
  (sset-class! root [:text :font] f))

(defn run []
  (-> (new-frame)
      (set-font (font :size 30))
      show!))
