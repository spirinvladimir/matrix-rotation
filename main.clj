(defn rotateOnly [r a] (drop r (take (+ (count a) r) (cycle a))))

(defn rotate [r a] (rotateOnly (mod r (count a)) a))

(defn cutSides [matrix]
  (reduce
    (fn [o line]
      [(conj (nth o 0) (first line))
       (conj (nth o 1) (last line))
       (conj (nth o 2) (drop-last (drop 1 line)))])
    [[] [] []]
    matrix))

(defn boundLines [ring matrix]
   (let [s (count matrix)]
     (if (= 0 s)
       [(reverse ring)]
       (let [right (take s ring)
             left (reverse (take-last s ring))
             down (reverse (drop-last s (drop s ring)))]
         (concat
           (map (fn [l c r] (concat [l] c [r])) left matrix right)
           [down])))))

(defn roll [r m s countRings]
  (let [rings r
        matrix m
        sizeLine s]
    (let [ring (last rings)]
      (if (= countRings 0)
        matrix
        (roll
          (drop-last rings)
          (concat
                  [(take sizeLine ring)]
                  (boundLines (drop sizeLine ring) matrix))

          (+ sizeLine 2)
          (dec countRings))))))

(defn output [matrix]
  (let [line (first matrix)
        tail (rest matrix)]
    (println (reduce (fn [s x] (str s " " x)) (first line) (rest line)))
    (if (> (count tail) 0)
      (output tail))))

(output
  (let [readArray (fn [] (map #(Integer/parseInt %) (clojure.string/split (read-line) #" ")))
        mnr (readArray)
        m (first mnr)
        n (second mnr)
        r (last mnr)
        min (if (< n m) n m)
        a (map #(readArray) (range m))]
    (loop [matrix a
           rings []
           countRings (dec (/ (if (odd? min) (dec min) min) 2))]
      (let [up (first matrix)
            down (last matrix)
            cutted (cutSides (drop-last (drop 1 matrix)))
            left (first cutted)
            right (second cutted)
            subMatrix (last cutted)
            ring (rotate r (concat up right (reverse down) (reverse left)))]
        (if (= countRings 0)
          (roll
            (conj rings ring)
            (map (fn [i] (vector)) (range (if (> m n) (- m n) 0)))
            (count up)
            countRings)
          (recur
            subMatrix
            (conj rings ring)
            (dec countRings)))))))
