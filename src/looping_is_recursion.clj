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
  -1)

(defn parity [a-seq]
  ":(")

(defn fast-fibo [n]
  ":(")

(defn cut-at-repetition [a-seq]
  [":("])