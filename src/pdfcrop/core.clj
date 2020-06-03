(ns pdfcrop.core
  (:gen-class)
  (:require 
   [swinghelp.core :refer [sset! sset-class! sget]]
   [clojure.java.io :as io]
   [clojure.java.shell :refer [sh with-sh-dir]]
   [pdfcrop.helpers :refer [swap-when! file-data]]
   [pdfcrop.java :refer
    [crop-pdf-impl pdf-file-data resize-image new-file-filter
     convert-to-image]])
  (:use [seesaw core font color graphics])
  (:import
   (java.io File)
   (javax.swing JFileChooser)
   (javax.swing.filechooser FileFilter FileNameExtensionFilter)
   (java.awt.image BufferedImage)
   (java.awt Canvas Dimension)
   (org.apache.pdfbox.pdmodel PDDocument PDPage)
   (org.apache.pdfbox.rendering PDFRenderer ImageType)
   (org.apache.pdfbox.pdmodel.common PDRectangle)))

;;FIXME: Check scaling.

;; state

(def plot-data {:height 500 :width 500})

;; src and dest are strings (not File objects)
(def state-init {:src nil
                 :dest nil
                 :page 0
                 :canvas-size [500 500]
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
  (let [bs (->> [[:crop "crop"][:view "view"][:close "close"]]
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
                    :east (label :id :info :class :text)
                    :south (page-buttons)))))

;; load

(defn- load-file-user
  "Returns the filename (string) selected by the user."
  [root]
  ;; erase the file object from JFileChooser ctor when finished .
  (let [chooser (doto (new JFileChooser (io/file "resources") )
                  (.setFileFilter (new-file-filter "pdf")))]
    (when  (-> chooser
               (.showOpenDialog root)
               (= JFileChooser/APPROVE_OPTION))
      (str (.getSelectedFile chooser)))))

(defmethod update-root-id :info [root _]
  (let [{:keys [start end]} @state]
    (sset! root [:info :text]
           (format "%s %s" start end))))

(defmethod update-root-id :src [root _]  
  (sset! root [:src :text]
         (-> @state :src io/file (.getName))))

(defmethod update-root-id :canvas [root _]
  (let [{:keys [src page start end canvas-size]} @state
        [w h] canvas-size]
    (when src
      (let [bimage (convert-to-image src page)
            paint (fn [c g]
                    (.setSize c (new Dimension w h))
                    (.drawImage g
                                (resize-image
                                 bimage
                                 w
                                 h)
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

(defn- put-cropped-filename [s]
  (str (subs s 0 (- (count s) (count ".pdf"))) "-cropped" ".pdf"))

;; FIXME : work on scaling

(defn- get-rect-coordinates
  [start end {[w h] :canvas-size src :src}]
  (let [flip-y (fn [[x y]] [x (- h y)])
        bimage (convert-to-image src 0)
        w* (.getWidth bimage)
        h* (.getHeight bimage)
        start (flip-y start)
        end (flip-y end)
        ratio-x (/ w* w)
        ratio-y (/ h* h)
        start (map * start [ratio-x ratio-y])
        end (map * end [ratio-x ratio-y])]
    [(apply min (map first [start end]))
     (apply min (map second [start end]))
     (apply max (map first [start end]))
     (apply max (map second [start end]))]))

(defn- crop-action [root]
  (if-not (can-crop? @state)
    (alert "cannnot crop")
    (let [{:keys [src start end canvas-size]} @state
          [lx ly rx ry] (get-rect-coordinates start end @state)
          src-file (io/file src)
          parent (.getParent src-file)
          new-file-name (put-cropped-filename src)
          new-file (io/file new-file-name)
          chooser (doto (new JFileChooser parent)
                    (.setDialogTitle "save file")
                    (.setSelectedFile new-file))]
      (if (= (.showSaveDialog chooser root) JFileChooser/APPROVE_OPTION)
        (let [selected-file  (.getSelectedFile chooser)]
          (crop-pdf-impl
           src-file
           selected-file
           [lx ly]
           [rx ry])
          (swap! state assoc :dest (str selected-file))
          (alert "cropped pdf"))))))

;; add behavior
(defn- add-behavior-button [root]
  (->> {:load (fn [e]
                (swap! state assoc :src (load-file-user root))
                (update-root root :src :canvas))
        :crop (fn [e] (crop-action root))
        :view (fn [e]
                (if-let [dest (:dest @state)]
                  (sh "open" dest)
                  (alert "crop a pdf first.")))
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
              (update-root root :canvas :info))
            :mouse-dragged
            (fn [e]
              (swap! state assoc :end (get-pos e))
              (update-root root :canvas :info))))
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

(defn -main
  [& args]
  (run))

