(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [x n]
                 (if (zero? n) 1
                   (if (== n 1) x
                     (recur (* base x) (dec n)))))]
    (helper base exp)))

(defn last-element [a-seq]
  (let [helper (fn [[a & rst :as a-seq]]
                 (if (empty? rst) a
                   (recur rst)))]
    (if (empty? a-seq) nil
      (helper a-seq))))

(defn seq= [seq1 seq2]
  (loop [[a & rst :as a-seq] seq1
         [b & rstb :as b-seq] seq2]
    (let [ase (empty? a-seq)
         bse (empty? b-seq)]
    (if (and ase bse) true
      (if (not (or ase bse))
        (if (not (= a b)) false
          (recur rst rstb))
        false)))))

(defn find-first-index [pred a-seq]
  (loop [i 0
         [a & rst :as a-s] a-seq]
    (if (empty? a-s) nil
      (if (pred a) i
        (recur (inc i) rst)))))

(defn avg [a-seq]
  (if (empty? a-seq) -1
    (loop [n 0
           sum 0
           [a & rst :as a-s] a-seq]
      (if (empty? a-s) (/ sum n)
        (recur (inc n) (+ sum a) rst)))))

(defn- toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (if (empty? a-seq) #{}
    (loop [wap #{}
           [a & rst :as a-s] a-seq]
      (if (empty? a-s) wap
        (recur (toggle wap a) rst)))))

(defn fast-fibo [n]
  (if (< n 2) n
    (loop [i (dec n)
           lst 0
           ths 1
           new 0]
      (if (zero? i) new
        (recur (dec i) ths (+ lst ths) (+ lst ths))))))

(defn cut-at-repetition [[a & rst :as a-seq]]
  (if (empty? a-seq) []
    (loop [seq1 []
           [b & rstb :as a-s] a-seq]
      (if (or (some #(= % b) seq1) (empty? a-s)) seq1
        (recur (conj seq1 b) rstb)))))

