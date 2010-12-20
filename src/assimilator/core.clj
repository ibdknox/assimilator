(ns assimilator.core)

(def options {:pop-size 100
              :cr-size 10
              :prob-crossover 60/100 })

(def problem-set {})

(defn edge-ex-rand [per top]
  (let [split (* top (- 1 (* 2 per)))
        low (* per top)]
    (+ low (rand-int split))))

(defn create-gene [] 
  (rand-nth (:pool problem-set)))

(defn gene-seq
  ([] (gene-seq (create-gene)))
  ([n] (lazy-seq (cons n (gene-seq (create-gene))))))

(defn create-chromosome []
  (take (:cr-size options) (gene-seq)))

(defn create-population []
  (repeatedly (:pop-size options) create-chromosome))

(defn crossover [c1 c2] 
  (let [slice (edge-ex-rand 10/100 (count c1))
        rc (shuffle [c1 c2])
        fc1 (first rc)
        fc2 (second rc)]
    (concat 
      (take slice fc1)
      (drop slice fc2))))

(defn probability-bool [perc] 
  (< (rand) perc))

(defn mate [c1 c2]
  (let [cross? (probability-bool (:prob-crossover options))]
    (if cross? 
      (crossover c1 c2)
      (rand-nth [c1 c2]))))

(defn random-mate [cpop] 
  ;; this should make sure self-mating can't happen
  (let [m1 (rand-nth cpop)
        m2 (rand-nth cpop)]
    (mate m1 m2)))

(defn refill-population [oldpop]
  (repeatedly (:pop-size options) #(random-mate oldpop)))

(defn run-ga [ps]
  (binding [problem-set ps]
    (let [population (filter (:valid-fn problem-set) (create-population))]
      (loop [cpop population
            num (:iter problem-set)]
        (let [pop-fits (sort (map (:fitness-fn problem-set) cpop))
              min-fit (first pop-fits)
              max-fit (last pop-fits)]
          (if (or (zero? num) (= (:end-fit problem-set) max-fit) (= max-fit min-fit))
            cpop
            (do
              ;;(println (str "Population " num " : [" max-fit ", " min-fit "]"))
              (recur (filter #((:valid-fn problem-set) % min-fit) (refill-population cpop))
                    (dec num)))))))))



