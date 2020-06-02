(ns pdfcrop.temp
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

(def from-file "resources/crop-test.pdf")

(def out-file "resources/out.pdf")

(defn get-bounding-coordinate
  [^File f]
  (let [box
        (-> f (PDDocument/load) (.getPage 0) (.getBBox))]
    [(.getLowerLeftX box)
     (.getLowerLeftY box)
     (.getUpperRightX box)
     (.getUpperRightY box)]))

(defn- get-num-pages
  "Given a pdf file, returns its number of pages."
  [^File f]
  (-> (PDDocument/load f) (.getNumberOfPages)))

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
  (let [[_ _ w h] (get-bounding-coordinate (io/file from-file))]
    (crop-pdf-impl from out
                   (map * lower-left [w h])
                   (map * upper-right [w h]))))

(crop-pdf-ratio-impl
 (io/file from-file)
 (io/file out-file)
 [0.4 0.5]
 [0.8 0.8])

(sh "open" out-file)

