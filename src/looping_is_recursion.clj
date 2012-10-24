(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [base exp acc] (if (== 1 exp)
                                    acc
                                    (recur base (dec exp) (* acc base))))]
    (if (== 0 exp)
       1
       (helper base exp base))))

(defn last-element [a-seq]
  (let [helper (fn [i a-seq] (if (== i (count a-seq))
                               (get a-seq (dec i))
                               (recur (inc i) a-seq)))]
    (helper 0 a-seq)))

(defn seq= [seq1 seq2]
  (if (not (== (count seq1) (count seq2)))
    false
    (loop [i 0]
      (cond
       (== i (count seq1)) true
       (not (= (first seq1) (first seq2))) false
       :else (recur (inc i))))))

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