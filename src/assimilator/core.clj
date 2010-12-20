(ns assimilator.core)

(def pool [{:name "probe" :damage 0}
           {:name "zealot" :damage 8}])

(defn edge-ex-rand [per top]
  (let [split (* top (- 1 (* 2 per)))
        low (* per top)]
    (+ low (rand-int split))))

(defn create-gene [] 
  (rand-nth pool))

(defn gene-seq
  ([] (gene-seq (create-gene)))
  ([n] (lazy-seq (cons n (gene-seq (create-gene))))))

(defn create-chromosome []
  (take 10 (gene-seq)))

(defn test-fitness [chromosome]
  (reduce + (map #(:damage %) chromosome)))

(defn create-population []
  (repeatedly 100 create-chromosome))

(defn crossover [c1 c2] 
  (let [slice (edge-ex-rand 10/100 (count c1))
        rc (shuffle [c1 c2])
        fc1 (first rc)
        fc2 (second rc)]
    (concat 
      (take slice fc1)
      (drop slice fc2))))

(defn valid
  ([chromosome] (valid chromosome 48))
  ([chromosome cur-min] 
   (> (test-fitness chromosome) cur-min)))

(defn probability-bool [perc] 
  (< (rand) perc))

(defn mate [c1 c2]
  (let [cross? (probability-bool 60/100)]
    (if cross? 
      (crossover c1 c2)
      (rand-nth [c1 c2]))))

(defn random-mate [cpop] 
  ;; this should make sure self-mating can't happen
  (let [m1 (rand-nth cpop)
        m2 (rand-nth cpop)]
    (mate m1 m2)))

(defn refill-population [oldpop]
  (repeatedly 100 #(random-mate oldpop)))

(defn run-ga []
  (let [population (filter valid (create-population))]
    (loop [cpop population
           num 9]
      (let [pop-fits (sort (map test-fitness cpop))
            min-fit (first pop-fits)
            max-fit (last pop-fits)]
        (if (or (zero? num) (= 80 max-fit))
          cpop
          (do
            (println (str "Population " num " : [" max-fit ", " min-fit "]"))
            (recur (filter #(valid % min-fit) (refill-population cpop))
                  (dec num))))))))



