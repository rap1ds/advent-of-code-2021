(ns day3
  (:require [util]))

(defn parse [lines]
  (map (fn [line] (map #(Character/getNumericValue %) line)) lines))

(defn bit-freqs [bits]
  (->> bits
       (apply map vector)
       (map frequencies)))

(defn most-common [bit-freqs]
  (map #(cond
          (= (% 0 0) (% 1 0)) 1
          (< (% 0 0) (% 1 0)) 1
          :else 0) bit-freqs))

(defn least-common [bit-freqs]
  (map #(cond
          (= (% 0 0) (% 1 0)) 0
          (< (% 0 0) (% 1 0)) 0
          :else 1) bit-freqs))

(defn to-dec [b]
  (Integer/parseInt (apply str b) 2))

(defn oxygen [bits]
  (loop [bits bits
         res []]
    (if (> (count bits) 1)
      (let [m (first (most-common (bit-freqs bits)))]
        (recur (->> bits
                    (filter #(= (first %) m))
                    (map rest))
               (conj res m)))
      (into res (first bits)))))

(defn co2 [bits]
  (loop [bits bits
         res []]
    (if (> (count bits) 1)
      (let [m (first (least-common (bit-freqs bits)))]
        (recur (->> bits
                    (filter #(= (first %) m))
                    (map rest))
               (conj res m)))
      (into res (first bits)))))

(comment
  ;; part 1
  (let [bit-freqs (->> "example/day3.txt"
                       util/read-lines
                       parse
                       bit-freqs)
        gamma (to-dec (most-common bit-freqs))
        epsilon (to-dec (least-common bit-freqs))]
    (* gamma epsilon))

  (let [bit-freqs (->> "input/day3.txt"
                       util/read-lines
                       parse
                       bit-freqs)
        gamma (to-dec (most-common bit-freqs))
        epsilon (to-dec (least-common bit-freqs))]
    (* gamma epsilon))

  ;; part 2
  (let [bits (->> "example/day3.txt"
                  util/read-lines
                  parse)]
    (* (to-dec
        (oxygen bits))
       (to-dec
        (co2 bits))))

  (let [bits (->> "input/day3.txt"
                  util/read-lines
                  parse)]
    (* (to-dec
        (oxygen bits))
       (to-dec
        (co2 bits))))
  )
