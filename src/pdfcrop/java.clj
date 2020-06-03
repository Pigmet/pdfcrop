(ns pdfcrop.java
  (:require 
   [swinghelp.core :refer [sset! sset-class! sget]]
   [clojure.string :refer [join]]
   [clojure.java.io :as io]
   [clojure.java.shell :refer [sh with-sh-dir]]
   [pdfcrop.helpers :refer [swap-when! file-data]])
  (:use [seesaw core font color graphics])
  (:import
   (java.io File)
   (javax.swing JFileChooser)
   (javax.swing.filechooser FileFilter FileNameExtensionFilter)
   (java.awt.image BufferedImage)
   (java.awt Canvas)
   (org.apache.pdfbox.pdmodel PDDocument PDPage)
   (org.apache.pdfbox.rendering PDFRenderer ImageType)
   (org.apache.pdfbox.pdmodel.common PDRectangle)))

(defn get-bounding-coordinate
  [^File f]
  (with-open [doc (PDDocument/load f)]
    (let [box (-> doc (.getPage 0) (.getBBox))]
      [(.getLowerLeftX box)
       (.getLowerLeftY box)
       (.getUpperRightX box)
       (.getUpperRightY box)])))

(defn- get-num-pages
  "Given a pdf file, returns its number of pages."
  [^File f]
  (with-open [doc (PDDocument/load f)]    
    (.getNumberOfPages doc)))

(defn pdf-file-data
  "Takes pdf file (File object), returns its data in a map with
  keys :width :height :pages."
  [^File f]
  (let [[_ _ w h ] (get-bounding-coordinate f)]
    {:width w :height h :pages (get-num-pages f)}))

(defn crop-pdf-impl
  "Crops from and save the result to out. They both should be
  pdf files. lower-left and upper-right are 2d points that determine
  the cropped area.  "
  [^File from ^File out lower-left upper-right]
  (let [[lx ly] lower-left, [rx ry] upper-right
        n (get-num-pages from)]
    (with-open [doc (PDDocument/load from)]
      (doseq [i (range 0 n)]
        (doto (.getPage doc i)
          ;; ctor of PDRectangle -> lower-left, upper-right
          (.setCropBox (new PDRectangle lx ly rx ry))))
      (.save doc out))))

(defn crop-pdf-ratio-impl
  "Like crop-pdf-impl, but lower-left and upper-right are
  coordinates in ratio (from 0 to 1).

  ^File from -> file to crop
  ^File out -> file to save the result"
  [^File from ^File out lower-left upper-right]
  (let [[_ _ w h] (get-bounding-coordinate (io/file from))]
    (crop-pdf-impl from out
                   (map * lower-left [w h])
                   (map * upper-right [w h]))))

(defn convert-to-image
  "Takes filepath and page number and returns BufferedImage."
  ([src n] (convert-to-image src n 300))
  ([src n dpi]
   (with-open [doc (PDDocument/load (io/file src))]
     (let [renderer (new PDFRenderer doc)]
       (.renderImageWithDPI renderer n dpi ImageType/RGB)))))

(defn resize-image [im new-width new-height]
  (let [temp (.getScaledInstance im
                                 new-width
                                 new-height
                                 BufferedImage/SCALE_SMOOTH)
        ret (buffered-image new-width new-height)
        g (.createGraphics ret)]
    (.drawImage g temp 0 0 nil)
    ret))

(defn bimage-data [^BufferedImage x]
  {:height (.getHeight x)
   :width (.getWidth x)})

(defn new-file-filter
  "Returns new JFileChooser that accept only files with extension."
  [extension]
  (proxy [FileFilter] []
    (accept [file]
      (or (.isDirectory file)
          (-> file str (.endsWith (str "." extension)))))
    (getDescription [] (format "%s files" extension))))

