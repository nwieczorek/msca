(ns msca.rule
  (:import  (java.awt Color Font))
  (:require [clojure.string :as string]
            [msca.utility :as ut])
  )



(def default-rules
  {:cell-width 1
   :cell-height 1
   :status-height 16
   :timer-milliseconds 100})

(defn read-rules
  [filename]
  (loop [lines (string/split-lines (slurp filename))
         prefix ""
         rule-map default-rules]
    (if (empty? lines)
      rule-map
      (let [line (first lines)
            lc (string/split line #"#")
            active-line (first lc)]   ;only process first token, as the rest is a comment
        (if (not-empty active-line)
          (let [tokens (map string/trim (string/split active-line #":"))]
            (cond (re-find #"^as " (first tokens))
                  (recur (rest lines) (second (string/split (first tokens) #"\s+")) rule-map)
                  (re-find #"^end " (first tokens))
                  (recur (rest lines) "" rule-map)
                  :else
                  (let [new-key (ut/keyword-append prefix (first tokens))
                        new-val (read-string (second tokens))]
                    (recur (rest lines) prefix (assoc rule-map new-key new-val)))))
          (recur (rest lines) prefix rule-map))))))


(defn rule-to-seq
  [ruleset key-prefix]
  (let [pat (re-pattern (str "^" key-prefix))]
    (map #(ruleset %) (sort (filter #(re-find pat (str %)) (keys ruleset))))))

(defn rule-to-array
  [ruleset key-prefix]
  (into [] (rule-to-seq ruleset key-prefix)))



(defn rule-to-color
  [ruleset key]
  (apply ut/to-color (ruleset key)))

(defn rule-to-color-array
  [ruleset key-prefix]
  (into [] (map #(apply ut/to-color %) (rule-to-seq ruleset key-prefix))))


(defn to-valid-state-lookup
  "Should return an integer or a keyword"
  [i]
  (if-let [po (keyword i)]
    po
    i))


(defn rule-to-update-function
  [ruleset key]
  (let [neighbors (+ (* 2 (ruleset :neighborhood-distance)) 1)
        raw-rule (ruleset key)]
    (eval 
      (list 'fn ['r 'g 'b] raw-rule))))

(defn rule-to-initial-function
  [ruleset key]
  (let [raw-rule (ruleset key)]
    (eval 
      (list 'fn [] raw-rule))))

(defn main
  [filename]
  (let [ruleset (read-rules filename)]
    (prn ruleset)
    (println)
    (prn ((rule-to-update-function ruleset :red-rule) [0] [0] [0]))
    ;(prn (rule-to-function ruleset :red-rule))
    ))
