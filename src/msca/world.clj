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

(defn make-cell-column
  [ruleset make-type]
  (let [cell-f  (cond
                  (= make-type :empty)  
                  make-cell
                  (= make-type :active) 
                  make-random-cell)]
    (into [] (repeatedly (ruleset :world-height) cell-f))))

(defn make-world
  [ruleset]
    { :cells (into [] 
                    (concat 
                      (list (make-cell-column ruleset :active))
                      (repeatedly (- (ruleset :world-width) 1) #(make-cell-column ruleset :empty))
                      ))
      :width (ruleset :world-width)
      :height (ruleset :world-height)
      :rules ruleset
    } 
   )

(defn get-state
  ([world x y]
   (assert (and (not (nil? x)) (not (nil? y))) (str "X " x " y " y))
   (assert (> (count (world :cells)) x) (str "X is out of range " (count (world :cells)) "," x))
   (assert (>= x 0) (str "x out of range:" x))
    (let [col ((world :cells) x)] 
      (assert (>= (count col) y) (str "Y is out of range " (count col) "," y))
      (col y)))
  ([world p]
   (let [[x y] p]
     (get-state world x y))))

(defn get-cell-state
  ([cells x y]
    ((cells x) y))
  ([cells p]
   (let [[x y] p]
     (get-cell-state cells x y))))


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
        red-func (rule/rule-to-function ruleset :red-rule)
        green-func (rule/rule-to-function ruleset :green-rule)
        blue-func (rule/rule-to-function ruleset :blue-rule)]
    (into []
      (for [y (range height)]
        (let [[rs gs bs] (get-neighbors input-col y height neighborhood )]
          [ (mod (red-func rs gs bs) 256)
            (mod (green-func rs gs bs) 256)
            (mod (blue-func rs gs bs)  256) ]
          )))))

(defn update
  [world]
  (let [input-col ((world :cells) 0)
        new-col (get-new-col (world :rules) input-col)
        new-cells (into [] (concat (list new-col)
                              (subvec (world :cells) 0 (- (world :width) 1))))]
    (assoc world :cells new-cells)))

    

