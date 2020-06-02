(ns pdfcrop.core
  (:gen-class)
  (:require 
   [swinghelp.core :refer [sset! sset-class! sget]]
   [clojure.java.io :as io]
   [clojure.java.shell :refer [sh with-sh-dir]]
   [pdfcrop.helpers :refer [swap-when! file-data]]
   [pdfcrop.java :refer [crop-pdf-ratio-impl pdf-file-data]])
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

(defn -main
  "I don't do a whole lot ... yet."
  [& args])

(defn convert-to-image
  "Takes filepath and page number and returns BufferedImage."
  ([src n] (convert-to-image src n 300))
  ([src n dpi]
   (with-open [doc (PDDocument/load (io/file src))]
     (let [renderer (new PDFRenderer doc)]
       (.renderImageWithDPI renderer n dpi ImageType/RGB)))))

(defn- resize-image [im new-width new-height]
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

(defn- new-file-filter
  "Returns new JFileChooser that accept only files with extension."
  [extension]
  (proxy [FileFilter] []
    (accept [file]
      (or (.isDirectory file)
          (-> file str (.endsWith (str "." extension)))))
    (getDescription [] (format "%s files" extension))))

;; state

(def plot-data {:height 500 :width 500})

;; src and dest are strings (not File objects)
(def state-init {:src nil
                 :dest :nil
                 :page 0
                 :start [0 0]
                 :end [0 0]})

(def state (atom state-init))

(defn- reset-state-id [& ids]
  (reset! state (merge @state (select-keys state-init ids))))

(defn- reset-state! [] (reset! state state-init))

(defn- state-ok?
  "Returns true if page is in the range of the src file."
  [{:keys [src page]}]
  (when src
    (let [{total-pages :pages} (pdf-file-data (io/file src))]
      (<= 0 page (dec total-pages)))))

(defmulti update-root-id (fn [root id] id))

(defn- update-root
  "Updates root in ks. Returns root"
  [root & ks]
  (dorun (map #(update-root-id root %) ks))
  root)

;; frame

(defn- ui-part []
  (let [bs (->> [[:crop "crop"][:close "close"] ]
                (map (fn [[id s]] (button :text s :id id :class :text))))

        part1  (horizontal-panel
                :items [(button :text "load" :id :load :class :text)
                        (menubar
                         :items
                         [(menu :text "menu" :class :text
                                :items bs)])])

        part2 (horizontal-panel
               :items [(label :text "File: " :class :text)
                       (label :id :src :class :text)])]
    
    (grid-panel :columns 1 :items [part1 part2])))

(defn- page-buttons []
  (grid-panel
   :rows 1
   :items
   (->>[[:back "back"][:next "next"]]
       (map (fn [[id s]]
              (button :text s :id id :class :text))))))

(defn- new-frame []
  (frame :width 800
         :height 800
         :title "crop pdf"
         :content (border-panel
                   :north (ui-part)
                   :center
                   (border-panel
                    :center(canvas :id :canvas)
                    :south (page-buttons)))))

;; load

(defn- load-file-user
  "Returns the filename (string) selected by the user."
  [root]
  ;; erase the file object from JFileChooser ctor when finished .
  (let [chooser (doto (new JFileChooser (new File "resources") )
                  (.setFileFilter (new-file-filter "pdf")))]
    (when  (-> chooser
               (.showOpenDialog root)
               (= JFileChooser/APPROVE_OPTION))
      (str (.getSelectedFile chooser)))))

(defmethod update-root-id :src [root _]  
  (sset! root [:src :text]
         (-> @state :src io/file (.getName))))

(defmethod update-root-id :canvas [root _]
  (let [{:keys [src page start end]} @state]
    (when src
      (let [bimage (convert-to-image src page)
            paint (fn [c g]
                    (.drawImage g
                                (resize-image bimage
                                              (width c)
                                              (height c))
                                0 0 nil)
                    (draw g
                          (apply rect
                                 (concat start (map - end start)) )
                          (style :foreground (color "black")
                                 :stroke 3)))]
        (sset! root [:canvas :paint] paint)))))

;; crop 

(defn- abs [x] (Math/abs (float x)))

(defn- valid-rect? [start end]
  (pos? (abs (apply * (map - start end)))))

(defn- can-crop? [{:keys [start end] :as the-state}]
  (and (state-ok? the-state) (valid-rect? start end)))

(defn- crop-action [root])

;; add behavior
(defn- add-behavior-button [root]
  (->> {:load (fn [e]
                (swap! state assoc :src (load-file-user root))
                (update-root root :src :canvas))
        :close (fn [e] (dispose! root))
        :next (fn [e]
                (swap-when! state state-ok? update :page inc)
                (update-root root :canvas))
        :back (fn [e]
                (swap-when! state state-ok? update :page dec)
                (update-root root :canvas))}
       (map (fn [[k v]]
              (listen (sget root k) :mouse-clicked v)))
       dorun)
  root)

(defn- add-draw-action [root]
  (let [get-pos (fn [e] [(.getX e)(.getY e)])]
    (listen (sget root :canvas)
            :mouse-pressed
            (fn [e] (swap! state
                           assoc
                           :start (get-pos e)
                           :end (get-pos e))
              (update-root root :canvas))
            :mouse-dragged
            (fn [e]
              (swap! state assoc :end (get-pos e))
              (update-root root :canvas))))
  root)

(defn- abs [x] (Math/abs (float x)))

(defn- rectangle-ok?
  "The rectangle [start end] has positive area?"
  [{:keys [start end]}]
  (pos? (abs (apply * (map - start end)))))

(defn- set-font [root f]
  (sset-class! root [:text :font] f))

(defn run []
  (reset-state!)
  (-> (new-frame)
      add-behavior-button
      add-draw-action
      (set-font (font :size 30))
      show!))

;;(run)


