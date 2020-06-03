(ns pdfcrop.temp
  (:use [seesaw core graphics font color]
        [swinghelp.core])
  (:require [pdfcrop.java :as impl]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh])
  (:import    (java.awt Canvas Dimension)))

(def sample-pdf "resources/Download.pdf")

(def out-pdf "resources/out.pdf")

(def state (atom {:src sample-pdf
                  :page 0
                  :scale 0.4
                  :start [0 0]
                  :end [0 0]
                  :pos [0 0]}))

(defn get-pdf-size [{:keys [src]}]
  (-> src io/file impl/pdf-file-data))

(defn- info-part []
  (vertical-panel
   :items (->> [:start :end :pdf-size :pos
                :rect-canvas]
               (map #(horizontal-panel
                      :items [(label :text (str (name %) ": ")
                                     :class :text)
                              (label :id % :class :text)])))))

(defn- make-frame []
  (frame :width 800
         :height 800
         :content
         (border-panel
          :south (info-part)
          :east (vertical-panel
                 :items [(button :text "crop" :id :crop :class :text)
                         (button :text "close" :id :close :class :text)])
          :center (canvas :id :canvas))))

(defmulti update-root-id (fn[root id] id))

(defn- update-root [root & ids]
  (dorun (map #(update-root-id root %) ids))
  root)

(defmethod update-root-id :pdf-size [root _]
  (let [{w :width h :height} (get-pdf-size @state)]
    (sset! root [:pdf-size :text] (format "[%s %s]" w h))))

(defn- flip-y  [[x y] &{:keys [h] :or {h 500}}] [x (- h y)])

(defmethod update-root-id :pos [root id]
  (let [h (-> @state :src io/file impl/pdf-file-data :height)]
    (sset! root [:pos :text] (format "%s"
                                     (-> @state :pos (flip-y :h h))))))

(defmethod update-root-id :start-end [root _]
  (let [{:keys [start end]} @state]
    (sset! root [:start :text]
           (format "[%s %s]" (first start) (second start)))
    (sset! root [:end :text]
           (format "[%s %s]" (first end) (second end)))))

(defn- get-rect-coordinate
  "Takes state, returns the cropping rectangle's coordinates
  relative to the canvas."
  [{:keys [start end src]}]
  (let[ h (-> src io/file impl/pdf-file-data :height)
       [start end] (map #(flip-y % :h h) [start end] )
       lx (apply min (map first [start end]))
       ly (apply min (map second [start end]))
       rx (apply max (map first [start end]))
       ry (apply max (map second [start end]))]
    [lx ly rx ry]))

(defmethod update-root-id :rect-canvas [root _]
  (sset! root [:rect-canvas :text]
         (format "%s"
                 (get-rect-coordinate @state))))

(defn- paint-image-and-rect
  "Takes current state value, returns paint fn."
  [{:keys [start end src page scale]}]
  (let [{w :width h :height} (-> src io/file impl/pdf-file-data)
        bimage (impl/convert-to-image src page)
        bimage* (impl/resize-image bimage w h)]
    (fn [c g]
      (.setSize c (new Dimension w h))
      (.drawImage g bimage* 0 0 nil)
      (draw g
            (apply rect
                   (concat start (map - end start)) )
            (style :foreground (color "black")
                   :stroke 3)))))

(defmethod update-root-id :canvas [root _]
  (sset! root [:canvas :paint]
         (paint-image-and-rect @state)))

(defn add-mouse-action [root]
  (let [get-pos (fn [e] [(.getX e)(.getY e)])]
    (listen (sget root :canvas)
            :mouse-pressed
            (fn [e]
              (swap! state assoc :start (get-pos e) :end (get-pos e))
              (update-root root :canvas :rect-canvas))
            :mouse-dragged
            (fn [e]
              (swap! state assoc :end (get-pos e))
              (update-root root :canvas :rect-canvas))
            :mouse-moved
            (fn [e] (swap! state assoc :pos (get-pos e))
              (update-root root :pos))))
  root)

(defn- add-button-action [root]
  (->>{:crop (fn [e]
               (let
                   [[lx ly rx ry] (get-rect-coordinate @state)]
                 (impl/crop-pdf-impl
                  (io/file (:src @state))
                  (io/file out-pdf)
                  [lx ly]
                  [rx ry])))
       :close (fn [e] (dispose! root))}
      (map (fn [[k v]]
             (listen (sget root k) :action v)))
      dorun)
  root)

(defn- run []
  (-> (make-frame)
      (update-root  :pdf-size :canvas)
      add-mouse-action
      add-button-action
      (sset-class! [:text :font] (font :size 30))
      (show!)))

;; (run)

;;(sh/sh "open" out-pdf)
