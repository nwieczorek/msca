(ns msca.world
  (:require [msca.utility :as utility]
            [msca.rule :as rule]))

(defn wrap-coordinate
  [x limit]
  (if (< x 0)
    (+ limit x)
    (if (>= x limit)
      (- x limit)
      x)))
;===================================================
; Points





(defn add-point
  [p1 p2]
  (let [[x1 y1] p1
        [x2 y2] p2]
    [(+ x1 x2) (+ y1 y2)]))

(defn rand-position
  [ruleset]
  [(rand-int (ruleset :world-width)) (rand-int (ruleset :world-height))])

(defn center-position
  [ruleset]
  [ (quot (ruleset :world-width) 2) (quot (ruleset :world-height) 2)])


;==========================================================
; World



(defn random-comp
  []
  (rand-int 255))

(defn make-cell
  []
  [0 0 0]
  )

(defn make-random-cell
  []
  [(random-comp) (random-comp) (random-comp)]
  )

(defn make-initial-column
  [ruleset]
  (let [red-func (rule/rule-to-initial-function ruleset :red-initial)
        green-func (rule/rule-to-initial-function ruleset :green-initial)
        blue-func (rule/rule-to-initial-function ruleset :blue-initial)
        cell-f  (fn [] [ (red-func) (green-func) (blue-func)])]
    (into [] (repeatedly (ruleset :world-height) cell-f))))


(defn make-world
  [ruleset]
    { :cells (make-initial-column ruleset)
      :width (ruleset :world-width)
      :height (ruleset :world-height)
      :rules ruleset
    } 
   )

(defn get-state
  [world y]
  (let [cells (world :cells)]
  (assert (>= (count cells) y) (str "Y is out of range " (count cells) "," y))
    (cells y)))



(defn get-neighbors
  "Input is the number of neighbors in one direction -- number of neighbors is 2n + 1
  Output is a vector of three vectors: r g b"
  [input-col y height neighborhood]
  (let [n-cells (for [abs_p (map #(wrap-coordinate % height) 
                                 (range (- y neighborhood) (+ y neighborhood 1)))]
                    (input-col abs_p))
        finish (fn [s] (into [] (reverse s)))]
    (loop [cells n-cells
          r-out '()
          g-out '()
          b-out '()]
     (if (empty? cells)
      (vector (finish  r-out) (finish g-out) (finish b-out))
      (let [[r g b] (first cells)]
        (recur (rest cells)
               (cons r r-out)
               (cons g g-out)
               (cons b b-out)))))))




(defn get-new-col
  [ruleset input-col]
  (let [height (ruleset :world-height)
        neighborhood (ruleset :neighborhood-distance)
        red-func (rule/rule-to-update-function ruleset :red-rule)
        green-func (rule/rule-to-update-function ruleset :green-rule)
        blue-func (rule/rule-to-update-function ruleset :blue-rule)]
    (into []
      (for [y (range height)]
        (let [[rs gs bs] (get-neighbors input-col y height neighborhood )]
          [ (mod (red-func rs gs bs) 256)
            (mod (green-func rs gs bs) 256)
            (mod (blue-func rs gs bs)  256) ]
          )))))

(defn update
  [world]
  (let [;start-time (System/currentTimeMillis)
        input-col (world :cells) 
        new-col (get-new-col (world :rules) input-col)
        end-time (System/currentTimeMillis)]
    ;(println "updated in " (- end-time start-time))
    (assoc world :cells new-col)))

    

