(ns pdfcrop.temp
  (:use [seesaw core graphics font color]
        [swinghelp.core])
  (:require [pdfcrop.java :as impl]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh])
  (:import    (java.awt Canvas Dimension)))

;; MEMO: I still don't figure out how to fix the scaling issue. Perhaps,
;; I should play with BufferedImage a bit before dealing with PDF. 

(def sample-pdf "resources/Download.pdf")
