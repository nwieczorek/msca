(ns msca.utility
  (:import   (java.awt Color)))

(defn strip-colon
  [s]
  (clojure.string/replace s ":" ""))

(defn keyword-append
  [kw-a kw-b]
  (if (= (str kw-a) "")
    (keyword (strip-colon (str kw-b)))
    (keyword (str (strip-colon (str kw-a)) "-" (strip-colon (str kw-b))))))

(defn to-color
  [r g b]
  (Color. r g b))


(defn- get-code-value
  [code args default]
  (let [code-match  (str "-" code)]
    (loop [i-args args]
      (if (empty? i-args) 
        (cond (= default :true) true 
              (= default :false) false
              :else (do (assert default (str "Required argument " code-match " not found"))
                      default))
        (if (= code-match (first i-args))
          (cond (= default :true) false
                (= default :false) true
                :else (do (assert (not-empty (rest i-args)) (str "Missing argument for " code-match))
                        (first (rest i-args))))
          (recur (rest i-args)))))))

(defn parse-args
  "arg-patterns is a sequence of patterns,
  Each pattern having the form: [ code description default ]
  A pattern with no default specified is required.
  A default of :true or :false indicates a unary code that does not require a following argument"
  [arg-patterns arg-list] 
  (loop [patterns arg-patterns
          args  arg-list
          arg-map {}]
    (if (empty? patterns)
      arg-map
      (let [[code description & defarg] (first patterns)
            default (if (empty? defarg) nil (first defarg))
            arg-val (get-code-value code args default) ]
        (recur (rest patterns) args (assoc arg-map (keyword code) arg-val)))))
  )

