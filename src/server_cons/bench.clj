(ns server-cons.bench
  (:require [server-cons.core :refer [allocate-machines allocate-machines*]]
            [server-cons.generators :as gen]
            [server-cons.combinatorics :as comb]))

(def machines [{:id 1 :cpu-avg 22}
                    {:id 2 :cpu-avg 17}
                    {:id 3 :cpu-avg 3}
                    {:id 4 :cpu-avg 3}
                    {:id 5 :cpu-avg 3}
                    {:id 6 :cpu-avg 3}
                    {:id 7 :cpu-avg 6}
                    {:id 8 :cpu-avg 6}
                    {:id 9 :cpu-avg 11}
                    {:id 10 :cpu-avg 35}
                    {:id 11 :cpu-avg 26}
                    {:id 12 :cpu-avg 29}
                    {:id 13 :cpu-avg 22}
                    {:id 14 :cpu-avg 3}
                    {:id 15 :cpu-avg 6}
                    {:id 16 :cpu-avg 11}
                    {:id 17 :cpu-avg 11}
                    {:id 18 :cpu-avg 35}
                    {:id 19 :cpu-avg 26}
                    {:id 20 :cpu-avg 29}
                    {:id 21 :cpu-avg 22}
                    {:id 22 :cpu-avg 6}
                    {:id 23 :cpu-avg 6}
                    {:id 24 :cpu-avg 11}
                    {:id 25 :cpu-avg 35}
                    {:id 26 :cpu-avg 26}
                    {:id 27 :cpu-avg 29}
                    {:id 28 :cpu-avg 22}
                    {:id 29 :cpu-avg 3}
                    {:id 30 :cpu-avg 6}
                    {:id 31 :cpu-avg 11}
                    {:id 32 :cpu-avg 11}
                    {:id 33 :cpu-avg 35}
                    {:id 34 :cpu-avg 26}
                    {:id 35 :cpu-avg 29}
                    {:id 36 :cpu-avg 22}
                    {:id 37 :cpu-avg 6}
                    {:id 38 :cpu-avg 6}
                    {:id 40 :cpu-avg 11}
                    {:id 41 :cpu-avg 35}
                    {:id 42 :cpu-avg 26}
                    {:id 43 :cpu-avg 29}
                    {:id 44 :cpu-avg 22}
                    {:id 45 :cpu-avg 3}
                    {:id 46 :cpu-avg 6}
                    {:id 47 :cpu-avg 11}
                    {:id 48 :cpu-avg 11}
                    {:id 49 :cpu-avg 35}
                    {:id 50 :cpu-avg 26}
                    {:id 51 :cpu-avg 29}
                    {:id 52 :cpu-avg 22}
                    {:id 53 :cpu-avg 6}
                    {:id 54 :cpu-avg 6}
                    {:id 55 :cpu-avg 11}
                    {:id 56 :cpu-avg 35}
                    {:id 57 :cpu-avg 26}
                    {:id 58 :cpu-avg 29}
                    {:id 59 :cpu-avg 22}
                    {:id 60 :cpu-avg 3}
                    {:id 61 :cpu-avg 6}
                    {:id 62 :cpu-avg 11}
                    {:id 63 :cpu-avg 11}
                    {:id 64 :cpu-avg 35}
                    {:id 65 :cpu-avg 26}
                    {:id 66 :cpu-avg 29}
                    {:id 67 :cpu-avg 22}
                    {:id 68 :cpu-avg 3}
                    {:id 69 :cpu-avg 6}
                    {:id 70 :cpu-avg 11}
                    {:id 71 :cpu-avg 35}
                    {:id 72 :cpu-avg 26}
                    {:id 73 :cpu-avg 29}
                    {:id 74 :cpu-avg 7}])

(defn score
  "Calculates a solution score. The lower the better"
  [max-cpu solution]
  (count solution))

(defn best-solution
  [max-cpu solutions]
  (->> solutions
       (map (juxt identity (partial score max-cpu)))
       (reduce #(if (> (second %1) (second %2)) %2 %1))
       first))

(defn bench-lazy-solutions
  [solutions-fn]
  (time
    (let [solutions (solutions-fn)]
      (do
        (time (println "solutions: " (count solutions)))
        (println "first: ")
        (println (first solutions))
        (println "best: ")
        (time (println (best-solution 60 solutions)))))))

(defn bench-first
  [solutions-fn]
  (time
    (let [solutions (solutions-fn)]
      (do
        (time (println "first: " (first solutions)))
        (println "score: " (score 60 (first solutions)))))))

(defn bench-comb*
  [n]
  (println "benchmark partition " n " machines")
  (let [machines (take n machines)
        partitions #(comb/allocate-by-partitions 60 machines)]
    (bench-lazy-solutions partitions)))

(defn bench-comb
  [n]
  (println "benchmark first partition " n " machines")
  (let [machines (take n machines)
        partitions #(comb/allocate-by-partitions 60 machines)]
    (bench-first partitions)))

(defn bench-logic*
  [n]
  (println "benchmark logic solutions " n " machines")
  (let [machines (take n machines)
        partitions #(allocate-machines* machines 60)]
    (bench-lazy-solutions partitions)))

(defn bench-logic
  [n]
  (println "benchmark first logic solution " n " machines")
  (let [machines (take n machines)
        partitions #(allocate-machines* machines 60)]
    (bench-first partitions)))

(defn bench-best-solution
  [n]
  (time
    (do
      (println "best solution for n: " n)
      (best-solution 60 (allocate-machines* (take n machines) 60)))))

(comment

  (count (combo/partitions (range 12))) ;4213597

  (doseq [n (range 10 13)]
    (println "n: " n)
    (bench-comb n)
    (bench-logic n))

  (bench-logic 4)
  (bench-logic 5)
  (bench-logic 7)
  (bench-logic 8)
  (bench-logic 10)
  (bench-best-solution 8)
  (bench-logic 12)
  (bench-logic 18)
  (bench-logic 19)
  (bench-logic 20)
  (bench-logic 30)
  (bench-logic 40)
  (bench-logic 50)

  (bench-comb 8)
  (bench-comb 9)
  (bench-comb 10)
  (bench-comb 11)
  (bench-comb 12)
  (bench-comb 18)
  (bench-comb 19)

  (let [machines (take 2 machines)]
    (enoughcpu 60 machines))

  (let [machines (take 2 machines)]
    (allocate-by-partitions 40 machines))

  (let [machines (take 5 machines)]
    (println "5 machines, partition"
    (allocate-by-partitions 40 machines)))

  (let [machines (vec (take 1 machines))]
    (allocate-machines machines 60))

  (let [machines (take 2 machines)]
    (allocate-machines machines 60))

  (let [machines (take 6 machines)]
    (time (allocate-machines machines)))

  (let [machines (take 8 machines)]
    (time (allocate-machines machines)))

  (let [machines (take 9 machines)]
    (time (allocate-machines machines)))

  (let [machines (take 10 machines)]
    (time (allocate-machines machines)))

  (let [machines (take 11 machines)]
    (time (allocate-machines machines)))

  (let [machines (take 12 machines)]
    (time (allocate-machines machines)))

  (let [machines (take 13 machines)]
    (time (allocate-machines machines)))

  (let [machines (take 14 machines)]
    (time (allocate-machines machines)))

  (let [machines (take 15 machines)]
    (time (allocate-machines machines)))

  (let [machines (take 18 machines)]
    (time (allocate-machines machines)))

  (let [machines (take 40 machines)]
    (time (allocate-machines machines)))  ; 708s

  (let [machines (take 45 machines)]
    (time (allocate-machines machines)))

  )
