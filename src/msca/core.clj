
(ns msca.core
  (:import (javax.swing JFrame Timer JPanel)
         (java.awt.event KeyListener ActionListener))
  (:use msca.utility)
  (:require [msca.rule :as rule]
            [msca.world :as world]
            [msca.gui :as gui]))

;=============================================================================
(defn msca-window
  [ruleset]
  (def frame (doto (JFrame. (str (ruleset :title)))
               (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
               ))

  (def panel (gui/msca-panel ruleset)) 

  (.setContentPane frame panel)
  (.validate frame)
  (.repaint frame)
  (.setVisible frame true)

  (let [insets (.getInsets frame)]
    (.setSize frame (+ (.left insets) (.right insets) (gui/get-display-width ruleset) ) 
                    (+ (.top insets) (.bottom insets) (gui/get-display-height ruleset) ))))
          
;=============================================================================
(def arg-patterns
  [ ["f" "Rule File"]
   ])

(defn main
  [& args]
  (let [pargs (parse-args arg-patterns args)
        ruleset (rule/read-rules (pargs :f))]
    (prn ruleset)
    (msca-window ruleset)
    ))


