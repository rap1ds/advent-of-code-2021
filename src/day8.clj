(ns day8
  (:require [util]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-line [line]
  (let [[signal output] (str/split line #" \| ")]
    (map #(str/split % #" ") [signal output])))

(defn parse [lines]
  (map parse-line lines))

(defn easy-digits [digits]
  (boolean (#{2 4 3 7} (count digits))))

(def count->num-candidate
  {2 #{1}
   3 #{7}
   4 #{4}
   5 #{2 3 5}
   6 #{0 6 9}
   7 #{8}})

(defn charset [signals]
  (map #(set (map identity %)) signals))

(defn solve-easy-signals [signal-charsets]
  (reduce
   (fn [acc signal]
     (update acc (count->num-candidate (count signal)) (fnil conj #{}) signal))
   {}
   signal-charsets))

(defn solve-069 [m]
  (let [one (first (m #{1}))
        zero-or-nine (set (filter #(set/superset? % one) (m #{0 6 9})))
        six (set/difference (m #{0 6 9}) zero-or-nine)
        four (first (m #{4}))
        nine (set (filter #(set/superset? % four) zero-or-nine))
        zero (set/difference zero-or-nine nine)]
    (assoc m
           #{0} zero
           #{6} six
           #{9} nine)))

(defn solve-235 [m]
  (let [one (first (m #{1}))
        three (set (filter #(set/superset? % one) (m #{2 3 5})))
        two-or-five (set/difference (m #{2 3 5}) three)
        four (first (m #{4}))
        five (set (filter #(= 3 (count (set/intersection four %))) two-or-five))
        two (set/difference two-or-five five)]
    (assoc m
           #{2} two
           #{3} three
           #{5} five)))

(defn solve-output [[signal output]]
  (let [signal-map (->> signal
                        charset
                        solve-easy-signals
                        solve-069
                        solve-235
                        (filter #(= 1 (count (key %))))
                        (map (fn [[a b]] [(first b) (first a)]))
                        (into {})
                        )]
    (parse-long (apply str (map signal-map (map set output))))))

(comment
  ;; part 1
  (->> "example/day8.txt"
       util/read-lines
       parse
       (map second)
       (map #(filter easy-digits %))
       flatten
       count) ; #=> 26

  (->> "input/day8.txt"
       util/read-lines
       parse
       (map second)
       (map #(filter easy-digits %))
       flatten
       count)

  (->> "example/day8-small.txt"
       util/read-lines
       parse
       first
       solve-output) ; #=> 5353
                                        ;
  (->> "example/day8.txt"
       util/read-lines
       parse
       (map solve-output)
       (reduce +)) ; #=> 61229

  (time
   ;; "Elapsed time: 18.735178 msecs"
   (->> "input/day8.txt"
        util/read-lines
        parse
        (map solve-output)
        (reduce +)))

  )
