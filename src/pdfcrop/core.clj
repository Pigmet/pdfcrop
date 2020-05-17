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

(defn- state->bimage []
  (let [{f :src n :page} @state
        {w :width h :height} plot-data]
    (when f
      (-> (convert-to-image f n)
          (resize-image w h)))))

(defmulti state->object (fn [id] id))

(defmethod state->object :bimage [_] (state->bimage))

(defmethod state->object :paint [_]
  (when-let [bimage (state->object :bimage)]
    (fn [c g]
      (let [{w :width h :height} plot-data
            {:keys [start end]} @state]
        (.setSize c w h)
        (.drawImage g bimage 0 0 nil)
        (draw g (apply rect (concat start (map - end start)))
              (style :foreground (color "black")
                     :stroke 4))))))

;; frame

(defn- buttons []
  (let [items (->> [:load :crop :back :next :view :close]
                   (map (fn [id] (button :text (name id) :id id
                                         :font (font :size 30)))))]
    (horizontal-panel :items items)))

(defn make-frame []
  (frame :width 800
         :height 800
         :content (border-panel
                   :id :main
                   :north (buttons)
                   :center (border-panel                            
                            :center (canvas :id :paint))
                   :south (vertical-panel
                           :items
                           (->> [:start :end :src]
                                (map #(label :id % :font (font :size 30))))))))

(defn get-pos[e] [(.getX e)(.getY e)])

(defmulti update-root-id (fn [root id] id))

(defn- update-root [root & ids]
  (dorun (map #(update-root-id root %) ids))
  root)

(defmethod update-root-id :default [root id]
  (sset! root [id :text] (format "%s %s" (name id) (id @state))))

(defmethod update-root-id :paint [root _]
  (sset! root [:paint :paint] (state->object :paint)))

(defn- new-file-filter [extension]
  (proxy [FileFilter] []
    (accept [file]
      (or (.isDirectory file)
          (-> file str (.endsWith (str "." extension)))))
    (getDescription [] (format "%s files" extension))))

(defn- rect-area [start end]
  (Math/abs (float (apply * (map - start end)))))

(defn- put-str-filename [f text]
  (let [{s :absolute-path ex :extension} (file-data f)]
    (->  (.substring s 0 (- (count s) (count ex)))
         (str text  ex))))

(defn- get-dest-file-user [root]
  (let [src (:src @state)
        {parent :parent s :name ex :extension} (file-data src)
        new-file-name (-> (put-str-filename src "-cropped")
                          file-data
                          :name
                          io/file)
        chooser (doto (new JFileChooser parent)
                  (.setDialogTitle "save file")
                  (.setSelectedFile new-file-name))]
    (if (= (.showSaveDialog chooser root) JFileChooser/APPROVE_OPTION)
      (str (.getSelectedFile chooser)))))

(defn- add-button-behavior[root]
  (->>{:load (fn [e]
               (let [chooser (doto (new JFileChooser )
                               (.setFileFilter (new-file-filter "pdf")))]
                 (when  (-> chooser
                            (.showOpenDialog root)
                            (= JFileChooser/APPROVE_OPTION))
                   (swap! state assoc :src (.getSelectedFile chooser))
                   (update-root root :src :paint))))

       :crop (fn [e]
               (let [{:keys [start end src]} @state
                     {w :width h :height} plot-data
                     start (map / start [w h])
                     end (map / end [w h])]
                 (when (and (pos? (rect-area start end)) src)
                   (let [dest (get-dest-file-user root)]
                     (crop-pdf-ratio-all-page
                      src dest start end)
                     (swap! state assoc :dest dest)))))

       :back (fn [e]
               (when
                   (swap-when! state state-ok? update :page dec)
                 (update-root root  :paint)))

       :next (fn [e]
               (when
                   (swap-when! state state-ok? update :page inc)
                 (update-root root :paint)))

       :view (fn [e]
               (when-let [dest (:dest @state)]
                 (sh "open" dest)))
       
       :close (fn [e] (dispose! root))}
      (map (fn [[k v]]
             (listen (sget root k) :mouse-clicked v)))
      dorun)
  root)

(defn- add-mouse-behavior [root]
  (listen (sget root :paint)
          :mouse-pressed
          (fn [e]
            (swap! state assoc :start (get-pos e) :end (get-pos e)))
          :mouse-dragged
          (fn [e]
            (swap! state assoc :end (get-pos e))
            (update-root root :paint)))
  root)

(defn- run []
  (reset-state!)
  (-> (make-frame)
      add-button-behavior
      add-mouse-behavior
      (update-root :start :end :paint)
      show!))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (run))
