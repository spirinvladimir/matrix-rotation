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

(defn myRoll [r m s]
  (let [rings r
        matrix m
        sizeLine s]
    (let [ring (last rings)]
      (if (nil? ring)
        matrix
        (myRoll
          (drop-last rings)
          (concat
                  [(take sizeLine ring)]
                  (boundLines (drop sizeLine ring) matrix))

          (+ sizeLine 2))))))

(defn output [matrix]
  (let [line (first matrix)
        tail (rest matrix)]
    (println (reduce (fn[s x](str s " " x)) (first line) (rest line)))
    (if (> (count tail) 0)
      (output tail))))

(output
  (let [mnr (map #(Integer/parseInt %) (clojure.string/split (read-line) #" "))
        m (nth mnr 0)
        n (nth mnr 1)
        r (nth mnr 2)
        min (if (< n m) n m)
        countRings (dec (/ (if (odd? min) (dec min) min) 2))
        a (map #(clojure.string/split (read-line) #" ") (range m))]
    (loop [matrix a
           rings []]
      (let [up (first matrix)
            down (last matrix)
            cutted (cutSides (drop-last (drop 1 matrix)))
            left (nth cutted 0)
            right (nth cutted 1)
            subMatrix (nth cutted 2)
            ring (rotate r (concat up right (reverse down) (reverse left)))]
        (if (= (count rings) countRings)
          (myRoll
            (conj rings ring)
            (map (fn [i] (vector)) (range (if (> m n) (- m n) 0)))
            (count up))
          (recur
            subMatrix
            (conj rings ring)))))))
