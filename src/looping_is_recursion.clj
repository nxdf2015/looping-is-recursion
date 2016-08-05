(ns looping-is-recursion)

;; (defn power-new [base exp]
;;   (cond
;;     (= 0 exp) 1
;;     :else (*  base  (power-new base (dec exp)))))



(defn power [base exp]
  (let [helper (fn [acc b]
                (if (zero? b)
                  acc
                  (recur (* acc base) (dec b))))]
    (helper 1 exp)))

;; (power 2 2)  ;=> 4
;; (power 5 3)  ;=> 125
;; (power 7 0)  ;=> 1
;; (power 0 10) ;=> 0


(defn last-element [a-seq]
 (first  (reverse a-seq)))

 (rest [])

;; (last-element [])      ;=> nil
;; (last-element [1 2 3]) ;=> 3
;; (last-element [2 5])   ;=> 5

(defn singleton? [liste]
  (empty? (rest liste)))


(defn new-last-element [a-seq]
  (let [helper (fn [ c ]
                 (if (singleton? c)
                   (first c)
                 (recur (rest c))))]
    (helper a-seq)))

;; (new-last-element [])      ;=> nil
;; (new-last-element [1 2 3]) ;=> 3
;; (new-last-element [2 5])   ;=> 5

(defn new-seq= [seq1 seq2]
  (cond
    (or
      (empty? seq1)
      (empty? seq2)) false
  :else (reduce (fn [c pair]
            (and(= (first pair)
                   (second pair))
                true))
                (map vector seq1 seq2))))
(defn listes-empty? [seq1 seq2]
  (and (empty? seq1)
       (empty? seq2)))

(defn xor [pred s1 s2]
  (let [ vs1 (pred s1)
        not-vs1 ((complement pred) s1)
        vs2 (pred s2)
        not-vs2 ((complement pred) s2) ]
    (or (and vs1 not-vs2) (and vs2 not-vs1))))

;; (xor even?  12  3);=>true
;; (xor even?  12  4);=>false

(defn seq= [seq1 seq2]
  (loop [ a seq1 b seq2]
    (cond
      (listes-empty? a b) true
      (xor empty? a b) false
      (= (first a) (first b))  (recur (rest a) (rest b))
      :else false)))


;; (seq= [1 2 4] '(1 2 4))  ;=> true
;; (seq= [1 2 3] [1 2 3 4]) ;=> false
;; (seq= [1 3 5] [])        ;=> false
;; (seq= [1 2 3 5 ] [1 3 4])


;; (defn find-first-index [pred a-seq]
;;   (loop [ index  0]
;;     (let [ elt (get a-seq index)]
;;       (cond
;;         (pred elt) index
;;         (empty? a-seq) nil
;;         :else (recur (inc index))))))


(defn find-first-index [pred a-seq]
   (loop [ loop-seq a-seq index 0]
      (cond
        (empty? loop-seq) nil
        (pred (first loop-seq)) index
        :else (recur (rest loop-seq) (inc index)))))


(find-first-index zero? [1 1 1 0 3 7 0 2])                    ;=> 3
(find-first-index zero? [1 1 3 7 2])                          ;=> nil
(find-first-index (fn [n] (= n 6)) [:cat :dog :six :blorg 6]) ;=> 4
(find-first-index nil? [])                                    ;=> nil

(defn avg [a-seq]
  (loop [ loop-seq a-seq sum 0 index 0]
     (if (empty? loop-seq)
       (/ sum index)
       (recur (rest loop-seq) (+ sum (first loop-seq)) (inc index)))))


;; (avg [1 2 3])   ;=> 2
;; (avg [0 0 0 4]) ;=> 1
;; (avg [1 0 0 1]) ;=> 1/2 ;; or 0.5

(defn parity [a-seq]
  (loop [ loop-seq a-seq set-parity {}]
    (cond
      (empty? loop-seq)
       (reduce  (fn [c x] (if (odd? (get set-parity x))
                            (conj c x)
                             c ))
        #{}
        (keys set-parity))
    :else (let [elt (first loop-seq)
                 new-set-parity (if (contains? set-parity elt )
                                      (assoc set-parity elt (inc (get set-parity elt)))
                                      (assoc set-parity elt 1))]
            (recur (rest loop-seq) new-set-parity)))))


;; (parity [:a :b :c])           ;=> #{:a :b :c
;; (parity [:a :b :c :a])        ;=> #{:b :c}
;; (parity [1 1 2 1 2 3 1 2 3 4]) ;=> #{2 4}


;; (defn fast-fibo [n]
;;   (if (= 0 n)
;;     0
;;     (loop [ k 1 fib { 0 0  1 1}]
;;       (cond
;;         (= k n) (get fib n)
;;         :else   (recur (inc k)
;;                     (assoc fib (inc k)
;;                        (+ (get fib  k) (get fib  (dec k)))))))))


(defn fast-fibo [n]
  (if (= 0 n)
    0
    (loop [ k 1 n- 1  n-- 0 ]
      (cond
        (= k n) n-
        :else   (recur (inc k) (+ n- n--) n-)))))


;; (fast-fibo 0) ;=> 0
;; (fast-fibo 1) ;=> 1
;; (fast-fibo 2) ;=> 1
;; (fast-fibo 3) ;=> 2
;; (fast-fibo 4) ;=> 3
;; (fast-fibo 5) ;=> 5
;; (fast-fibo 6) ;=> 8

(defn include? [liste elt]
  (loop [loop-liste liste]
    (cond
      (empty? loop-liste) false
      (= (first loop-liste) elt) true
      :else (recur (rest loop-liste)))))

(defn cut-at-repetition [a-seq]
  (loop [ loop-seq a-seq a-list []]
    (cond
      (empty? loop-seq) (into [] a-list)
      :else (let [ elt (first loop-seq)
                   new-list (if (include?  a-list elt)
                                  a-list
                                  (conj a-list elt))]
              (recur (rest loop-seq) new-list)))))


;;  (cut-at-repetition [1 1 1 1 1])
;; ;; ;=> [1] doesn't have to be a vector, a sequence is fine too
;;  (cut-at-repetition [:cat :dog :house :milk 1 :cat :dog])
;; ;; ;=> [:cat :dog :house :milk 1]
;;  (cut-at-repetition [0 1 2 3 4 5])
;; ;; ;=> [0 1 2 3 4 5]











