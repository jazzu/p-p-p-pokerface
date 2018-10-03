(ns p-p-p-pokerface)

(def high-cards {\T 10 \J 11 \Q 12 \K 13 \A 14})

(defn high-card-value [r]
  (get high-cards r))


(defn rank [card]
  (let [[r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (high-card-value r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn sorted-frequencies [hand]
  (sort (vals (frequencies (map rank hand)))))

(defn max-feature [f hand]
  (apply max(vals (frequencies (map f hand)))))

(defn high-card? [hand]
  true)

(defn pair? [hand]
  (== 2
      (max-feature rank hand)))

(defn three-of-a-kind? [hand]
  (== 3
      (max-feature rank hand)))

(defn four-of-a-kind? [hand]
  (== 4
      (max-feature rank hand)))

(defn flush? [hand]
  (== 5
      (max-feature suit hand)))

(defn full-house? [hand]
  (= [2 3]
     (sorted-frequencies hand)))

(defn two-pairs? [hand]
  (= [1 2 2]
     (sorted-frequencies hand)))

(defn straight? [hand]
  (let [sorted-hand (sort (map rank hand))
        sorted-hand-low-ace (sort (replace {14 1} (map rank hand)))
        possible-straight (range (first sorted-hand) (+ (first sorted-hand) 5))
        possible-straight-low-ace [1 2 3 4 5]]
    (or (= sorted-hand possible-straight)
        (= sorted-hand-low-ace possible-straight-low-ace))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn value [hand]
  (let [checkers [[straight-flush? 8] [four-of-a-kind? 7]
                  [full-house? 6] [flush? 5]
                  [straight? 4] [three-of-a-kind? 3]
                  [two-pairs? 2] [pair? 1]
                  [high-card? 0]]
        check (fn [c]
                (if ((first c) hand)
                  (second c)
                  0))]

    (apply max (map check checkers))))
