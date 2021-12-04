(ns day4
  (:require [util]
            [clojure.string :as str]))

(defn parse-board [board]
  (->> board
       (map #(str/split % #"\s"))
       (map #(filter seq %))
       (map #(map parse-long %))))

(defn with-transposed-board [board]
  [board (apply map vector board)])

(defn parse-boards [boards]
  (->> boards
       (partition-by empty?)
       (filter (comp seq first))
       (map parse-board)
       (map with-transposed-board)))

(defn parse-numbers [numbers]
  (map parse-long (str/split numbers #",")))

(defn parse [lines]
  (let [numbers (first lines)
        boards (rest lines)]
    {:numbers (parse-numbers numbers)
     :boards (parse-boards boards)}))

(defn board-wins? [board]
  (some empty? board))

(defn wins? [boards]
  (let [[b1 b2] boards]
    (or (board-wins? b1)
        (board-wins? b2))))

(defn remove-num [n [b1 b2]]
  [(map #(remove (fn [bn] (= n bn)) %) b1)
   (map #(remove (fn [bn] (= n bn)) %) b2)])

(defn draw [{:keys [numbers boards drawn]}]
  {:drawn (conj drawn (first numbers))
   :numbers (rest numbers)
   :boards (->> boards
                (remove wins?)
                (map #(remove-num (first numbers) %)))})

(defn draw-until-win [ctx]
  (when (seq (:numbers ctx))
    (if (some wins? (:boards ctx))
      ctx
      (recur (draw ctx)))))

(defn draw-until-every-win [ctx]
  (when (seq (:numbers ctx))
    (if (every? wins? (:boards ctx))
      ctx
      (recur (draw ctx)))))

(defn score [ctx]
  (* (first (:drawn ctx))
     (apply +
            (->> (:boards ctx)
                 (filter wins?)
                 ffirst
                 flatten))))

(comment
  ;; part 1
  (->>
   (parse
    (util/read-lines "example/day4.txt"))
   draw-until-win
   score)

  (time
   ;; "Elapsed time: 17.503939 msecs"
   (->>
    (parse
     (util/read-lines "input/day4.txt"))
    draw-until-win
    score))

  ;; part 2
  (->>
   (parse
    (util/read-lines "example/day4.txt"))
   draw-until-every-win
   score)

  (time
   ;; "Elapsed time: 33.483457 msecs"
   (->>
    (parse
     (util/read-lines "input/day4.txt"))
    draw-until-every-win
    score)))
