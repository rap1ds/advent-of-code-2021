(ns day9
  (:require [util]))

(defn parse [lines]
  (map #(map (comp parse-long str) %) lines))

(defn xy-lookup-map [rows]
  (into {}
        (apply concat
               (map-indexed (fn [y cols]
                              (map-indexed (fn [x item] [[x y] item]) cols)) rows))))

(defn adjacent [[x y] w h]
  (let [top [x (dec y)]
        right [(inc x) y]
        bottom [x (inc y)]
        left [(dec x) y]]
    (cond-> []
      (<= 0 (dec y)) (conj top)
      (< (inc x) w) (conj right)
      (< (inc y) h) (conj bottom)
      (<= 0 (dec x)) (conj left))))

(defn low-point? [[[x y] v] lookup-map w h]
  (every? #(< v %) (map lookup-map (adjacent [x y] w h))))

(defn risk [low-points]
  (reduce + (map #(inc (second %)) low-points)))

(defn basin [[[x y] v] lookup-map w h]
  (let [adjs (adjacent [x y] w h)]
    (set
     (conj
      (->> adjs
           (filter (fn [[x' y']]
                     (and
                      (not= 9 (lookup-map [x' y']))
                      (< v (lookup-map [x' y'])))))
           (mapcat #(basin [% (lookup-map %)] lookup-map w h)))
      [x y]))))

(defn basin-size [basin]
  (count basin))

(comment
  ;; part1
  (let [heightmap (-> "example/day9.txt"
                      util/read-lines
                      parse)
        h (count heightmap)
        w (count (first heightmap))
        lookup-map (xy-lookup-map heightmap)]
    (->> lookup-map
         (filter (fn [[[x y] v]] (low-point? [[x y] v] lookup-map w h)))
         risk))

  (time
   ;; "Elapsed time: 33.549557 msecs"
   (let [heightmap (-> "input/day9.txt"
                       util/read-lines
                       parse)
         h (count heightmap)
         w (count (first heightmap))
         lookup-map (xy-lookup-map heightmap)]
     (->> lookup-map
          (filter (fn [[[x y] v]] (low-point? [x y] v lookup-map w h)))
          risk)))

  ;; part2
  (let [heightmap (-> "example/day9.txt"
                      util/read-lines
                      parse)
        h (count heightmap)
        w (count (first heightmap))
        lookup-map (xy-lookup-map heightmap)
        low-points (->> lookup-map
                        (filter (fn [[[x y] v]] (low-point? [[x y] v] lookup-map w h))))]
    (->> low-points
         (map #(basin % lookup-map w h))
         (map basin-size)
         (sort)
         (reverse)
         (take 3)
         (reduce *))) ; #=> 1134


  (time
   ;; "Elapsed time: 129.185522 msecs"
   (let [heightmap (-> "input/day9.txt"
                       util/read-lines
                       parse)
         h (count heightmap)
         w (count (first heightmap))
         lookup-map (xy-lookup-map heightmap)
         low-points (->> lookup-map
                         (filter (fn [[[x y] v]] (low-point? [[x y] v] lookup-map w h))))]
     (->> low-points
          (map #(basin % lookup-map w h))
          (map basin-size)
          (sort)
          (reverse)
          (take 3)
          (reduce *))))

  )
