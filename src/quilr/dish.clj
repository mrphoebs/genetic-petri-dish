(ns quilr.dish
  (:use quil.core))

(def nutrition-value 1)
(def injury-value 1)
(def mutable-genes 100)

(defn is-living-cell? [dish x y]
  "checks wether the cell in dish at position x, y is non zero/living"
  (not (= (nth (nth dish y) x) 0)))

(defn fn-dish [f dish]
  "applies a side effecting function f on each point in the dish"
  ()
(doseq [y (take (count dish) (range) ) x (take (count (nth dish 0)) (range))]    (f dish x y)))

(defn draw-dish [dish x y]
  "Draws points on a dish (2d list) that are not 0"
    (if (is-living-cell? dish x y)  (point x y)))

(defn aggregate-fn-dish [f dish]
  (reduce + (for [y (take (count dish) (range) ) x (take (count (nth dish 0)) (range))] 
    (f dish x y))))

(defn number-of-surrounding-living-cells [dish x y]
  (reduce +
      (for [x1 [(dec x) x (inc x)] y1 [(dec y) y (inc y)]]
           (if (or 
                 (< x1 0)
                 (< y1 0)
                 (< (dec (count dish)) x1)
                 (< (dec (count dish)) y1)
                 (not (is-living-cell? dish x1 y1)))
             1 
             0))))

(defn injuryfn [dish x y]
  (if (is-living-cell? dish x y)
    (* (number-of-surrounding-living-cells dish x y) injury-value)
    0))

(defn injury-score [dish x y]
  (injuryfn dish x y))

(defn nutrition-score [dish x y]
  (if (is-living-cell? dish x y) nutrition-value 0))

(defn fitness-score [dish]
  (- (aggregate-fn-dish nutrition-score dish) (aggregate-fn-dish injury-score dish)))

(defn mutate-gene [dish x y]
  (assoc dish x 
         (assoc (nth dish x) y 
                (if (is-living-cell? dish x y) 0 1))))

(defn mutate-dish [dish number-of-genes]
  (if (= number-of-genes 0) 
    dish 
    (mutate-dish 
      (mutate-gene dish (rand-int (count dish)) (rand-int (count dish))) 
      (dec number-of-genes))))

(defn select-best [n dishes]
  (shuffle (take-last n (sort-by fitness-score dishes))))

(defn create-generation [dish population genes-to-mutate]
  (take population (repeatedly #(mutate-dish dish genes-to-mutate))))

(defn cross-over [dish1 dish2]
  (concat (into [] (take (int (/ (count dish1) 2)) dish1))
          (into [] (take-last (- (count dish2) (int (/ (count dish2) 2))) dish2))))

(defn get-cross-overs [dishes]
  (concat
    (map cross-over
         (nth (into [] (partition (/ (count dishes) 2) dishes)) 0)
         (nth (into [] (partition (/ (count dishes) 2) dishes)) 1))
    (map cross-over
         (nth (into [] (partition (/ (count dishes) 2) dishes)) 1)
         (nth (into [] (partition (/ (count dishes) 2) dishes)) 0))))


(defn empty-dish [size]
 "creates a square world of size x size" 
  (into [] (repeat size (into [] (repeat size 0)))))

(defn genetic-algo [[first & rest]]
  (println first)
  (genetic-algo
  (concat rest (get-cross-overs 
    (select-best 4 (repeatedly 4 
                        (fn [] (mutate-dish (into [] first) 10))))))))
