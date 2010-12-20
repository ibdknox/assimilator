(ns assimilator.runner
  (:use assimilator.core ))

(def pool [{:name "probe" :damage 0}
           {:name "zealot" :damage 8}
           {:name "stalker" :damage 14}])

(defn test-fitness [chromosome]
  (reduce + (map #(:damage %) chromosome)))

(defn valid
  ([chromosome] (valid chromosome 75))
  ([chromosome cur-min] 
   (> (test-fitness chromosome) cur-min)))

(def params {:pool pool 
             :fitness-fn test-fitness 
             :valid-fn valid 
             :end-fit 150 
             :iter 50})

(defn view-result [result]
  (println "Fitness level: " (test-fitness result))
  (map (fn [order]
         (println (:name order)))
       result))

(defn exec [] 
  (let [results (distinct (run-ga params))]
    (map view-result results)))


