(ns day7
  (:require [util]
            [clojure.string :as str]))

(defn parse [lines]
  (map parse-long (str/split (first lines) #",")))

(defn median [n]
  (first (drop (/ (count n) 2) n)))

(defn distance [a b]
  (Math/abs (- a b)))

(defn moves-needed [positions target]
  (->> positions
       (map (partial distance target))))

(defn fuel-consumption1 [moves]
  (reduce + moves))

(defn fuel-consumption2 [moves]
  (->> moves
       (map #(* (/ (inc %) 2) %))
       (reduce +)))

(defn part1 [nums]
  (-> nums
      sort
      (moves-needed (median nums))
      fuel-consumption1))

(defn part2 [nums]
  (let [nums (sort nums)]
    (second
     (reduce
      (fn [acc target]
        (if acc
          (let [[_ old-cons] acc
                new-cons (-> nums
                             (moves-needed target)
                             fuel-consumption2)]
            (if (< new-cons old-cons)
              [target new-cons]
              acc))
          [target (-> nums
                      (moves-needed target)
                      fuel-consumption2)]))
      nil
      (range (apply max nums))))))

(comment
  ;; part 1
  (part1 (parse (util/read-lines "example/day7.txt"))) ; #=> 37
                                        ;
  (part1 (parse (util/read-lines "input/day7.txt"))) ; #=> 37

  (time
   ;; "Elapsed time: 6.618044 msecs"
   (let [nums (sort (parse (util/read-lines "input/day7.txt")))]
     (-> nums
         (moves-needed (median nums))
         fuel-consumption1)))

  ;; part2
                                        ;
  (part2 (parse (util/read-lines "example/day7.txt"))) ; #=> 168N

  (time
   ;; "Elapsed time: 9031.747836 msecs"
   (part2 (parse (util/read-lines "input/day7.txt"))))

  )
