(ns pdfcrop.helpers
  (:require [clojure.java.io :as io]
            [cljrx.core :as r]
            [clojure.string :refer [split replace]]))

(defn swap-when!
  "Swaps the value of a if the result satisfies pred, or returns nil."
  [a pred f & args]
  (let [new-val (apply f @a args)]
    (when (pred new-val)
      (reset! a new-val))))

(defn- get-extension [f]
  (re-find #"\.\w+" (str f)))

;; FIXME: This is buggy ->  core.clj.tex 

(defn- filename-sans-extension [f]
  (first (split (str f) #"\.\w+" )))

(defn file-data [f]
  (let [f (if (string? f) (io/file f) f)
        parent (.getParent f)
        siblings (->> parent io/file (.listFiles) (map str))]
    {:parent parent
     :directory? (.isDirectory f)
     :name (.getName f)
     :absolute-path (.getAbsolutePath f)
     :siblings siblings
     :extension (re-find #"\.\w+" (str f))}))

