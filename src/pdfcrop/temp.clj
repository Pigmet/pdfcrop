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

(defn- crop-pdf-impl [^File from ^File out lower-left upper-right]
  (let [[lx ly] lower-left, [rx ry] upper-right
        n (get-num-pages from)]
    (with-open [doc (PDDocument/load from)]
      (doseq [i (range 0 n)]
        (doto (.getPage doc i)
          ;; ctor of PDRectangle -> lower-left, upper-right
          (.setCropBox (new PDRectangle lx ly rx ry))))
      (.save doc (new File "resources/out.pdf")))))

(crop-pdf-impl (io/file from-file)
               (io/file out-file)
               [20 20]
               [300 400])

(get-num-pages (io/file out-file))

(sh "open" out-file)


