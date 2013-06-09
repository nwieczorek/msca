
(ns msca.gui
  (:import (javax.swing JFrame Timer JPanel)
         (java.awt.event KeyListener ActionListener)
         (java.awt.image BufferedImage)
         (java.awt Color RenderingHints Toolkit Image Font))
  (:use msca.utility)
  (:require [msca.rule :as rule]
            [msca.world :as world]
            [msca.utility :as ut]))




;=============================================================================
(defn get-text-height
  [^java.awt.Graphics2D g2d]
  (let [frc (.getFontRenderContext g2d)
        lm (.getLineMetrics (.getFont g2d), "A", frc )]
    (int (.getAscent lm))))

(defn translate-coords
  [x y cell-width cell-height]
  [ (* x cell-width) (* y cell-height)])


(defn make-color-map
  []
  (atom {}))

(defn get-color
  ([cmap r g b]
    (let [ck (str r "," g "," b)]
      (if (contains? cmap ck)
       (@cmap ck)
       (let [new-color (to-color r g b)]
        (swap! cmap #(assoc % ck new-color))
        new-color))))
  ([cmap cvec]
   (let [[r g b] cvec]
     (get-color cmap r g b))))
  


(defn msca-panel
  [ruleset ]
  (let [cell-width (ruleset :cell-width)
        cell-height (ruleset :cell-height)
        world-height (ruleset :world-height)
        column-height (* cell-height world-height)
        text-x (* 2 cell-width)
        text-y (+ (* (ruleset :world-height) cell-height) 1)
        world-colors (make-color-map)
        world (atom (world/make-world ruleset))
        columns (atom '())
        counter (atom 0)
        pnl (doto (proxy [javax.swing.JPanel] []
                    (paintComponent [^java.awt.Graphics g]
                      (proxy-super paintComponent g)
                      (let [g2d (doto ^java.awt.Graphics2D
                                  (.create g))
                            text-height (get-text-height g2d)
                            ;start-time (System/currentTimeMillis)
                            ]
                        ;Draw the new image for the new column 
                        (let [bimg (BufferedImage. cell-width column-height BufferedImage/TYPE_INT_RGB)
                              gr (.getGraphics bimg)]
                          (doseq [y (range (ruleset :world-height))]
                            (let [state (world/get-state @world y)
                                  [rx ry] (translate-coords 0 y cell-width cell-height)]
                              (.setColor gr  (get-color world-colors state))
                              (.fillRect gr rx ry cell-width cell-height)))
                           ;Add the new image to the column list
                           (swap! columns (fn [cols] (cons bimg cols)))  
                          )
                        ;Draw all columns
                        (let [indexed-cols (map #(vector %1 %2) (range (count @columns)) @columns)]
                          (doseq [[x img] indexed-cols]
                            (let [[rx ry] (translate-coords x 0 cell-width cell-height)] 
                              (.drawImage g2d img rx ry this)
                          )))

                        ;(println "drew in " (- (System/currentTimeMillis) start-time))
                        (.setColor g2d Color/BLACK)
                        (.drawString g2d (str "Iteration " @counter) text-x (+ text-height text-y))

                        ))
                    ))
        timer (Timer. (ruleset :timer-milliseconds)
                      (proxy [ActionListener] []
                        (actionPerformed [event]
                          (reset! world (world/update @world))
                          (reset! counter (inc @counter))
                          (.repaint pnl)
                          )))]
    (.start timer)
    pnl
    ))



(defn get-display-width
  [ruleset]
  (* (ruleset :world-width) (ruleset :cell-width) ))

(defn get-display-height
  [ruleset]
  (+ (* (ruleset :world-height) (ruleset :cell-height))
    (ruleset :status-height))) 
        
