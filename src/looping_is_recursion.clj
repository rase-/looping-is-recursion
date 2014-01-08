(ns looping-is-recursion)

(defn power [base exp]
  (if (zero? exp) 
    1
    (* base (power base (dec exp)))))

(defn last-element [a-seq]
  (let [fst (first a-seq)
        rst (rest a-seq)]
    (if (empty? rst)
      fst
      (recur rst))))

(defn seq= [seq1 seq2]
  (let [x (first seq1)
        y (first seq2)
        rst1 (rest seq1)
        rst2 (rest seq2)]
    (cond
      (and (empty? rst1) (empty? rst2)) true
      (or (empty? rst1) (empty? rst2)) false
      (== x y) (recur rst1 rst2)
      :else false)))

(defn find-first-index [pred a-seq]
  (loop [i 0]
    (cond 
     (== i (count a-seq)) nil
     (pred (get a-seq i)) i
     :else (recur (inc i)))))

(defn avg [a-seq]
  (loop [i 0
         sum 0]
    (if (== i (count a-seq))
      (/ sum (count a-seq))
      (recur (inc i) (+ sum (get a-seq i))))))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (loop [i 0
         s #{}]
    (if (== i (count a-seq))
      s
      (recur (inc i) (toggle s (get a-seq i))))))

(defn fast-fibo [n]
  (cond
   (== n 0) 0
   (== n 1) 1
   :else (loop [fn-2 0
                fn-1 1
                i 2]
           (if (== i n)
             (+ fn-2 fn-1)
             (recur fn-1 (+ fn-2 fn-1) (inc i))))))

(defn cut-at-repetition [a-seq]
  (loop
    [i 0
     seen []]
    (cond
     (== i (count a-seq)) a-seq
     (some (fn [x] (= x (get a-seq i))) seen) seen
     :else (recur (inc i) (conj seen (get a-seq i))))))
