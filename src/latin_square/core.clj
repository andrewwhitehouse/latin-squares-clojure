(ns latin-square.core
  (:require [clojure.data.int-map :as i]))

(defn square? [grid]
  (apply = (count grid) (map count grid)))

(defn square-values [square]
  (let [size (count square)
        rowcol-values (object-array (map (fn [_] (i/int-set)) (range (* 2 size))))]
    (doseq [ri (range size)
            ci (range size)]
      (let [value (get-in square [ri ci] :not-found)]
        (if (= :not-found value)
          (throw (ex-info "Missing value"
                          {:cause        :missing-value
                           :row-index    ri
                           :column-index ci})))
        (if (or (<= value 0) (> value size))
          (throw (ex-info "Value out of range"
                          {:cause :out-of-range
                           :value value})))
        (aset rowcol-values ri (conj (aget rowcol-values ri) value) )
        (aset rowcol-values (+ size ri) (conj (aget rowcol-values (+ size ri)) value))))
    (let [result (seq rowcol-values)]
      {:row-values (take size result) :column-values (drop size result)})))


(defn latin-square?
  ([grid] (latin-square? square-values grid))
  ([square-value-f grid]
   (if (not (square? grid))
     (do
       (println "Not a square")
       false)
     (try
       (let [row-col-values (square-value-f grid)
             all-counts (->> row-col-values (mapcat val) (map count))]
         (if-let [first-count (first all-counts)]
           (every? #(= first-count %) (rest all-counts))
           false))
       (catch Exception e
         (println (.getMessage e))
         false)))))


(defn -main [& args]
  (println (latin-square? [[1 2 3] [2 3 1] [3 1 2]])))


