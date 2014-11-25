(ns server-cons.bench
  (:require [server-cons.core :refer [allocate-machines allocate-machines*]]
            [server-cons.generators :as gen]
            [server-cons.combinatorics :as comb]))

(def machines [{:id 1 :cpu-avg 22}
               {:id 2 :cpu-avg 17}
               {:id 3 :cpu-avg 6}
               {:id 4 :cpu-avg 17}
               {:id 5 :cpu-avg 6}
               {:id 6 :cpu-avg 17}
               {:id 7 :cpu-avg 6}
               {:id 8 :cpu-avg 17}
               {:id 9 :cpu-avg 6}
               {:id 10 :cpu-avg 17}
               {:id 11 :cpu-avg 6}
               {:id 12 :cpu-avg 17}
               {:id 13 :cpu-avg 6}
               {:id 14 :cpu-avg 6}
               {:id 15 :cpu-avg 17}
               {:id 16 :cpu-avg 6}
               {:id 17 :cpu-avg 17}
               {:id 18 :cpu-avg 6}
               {:id 19 :cpu-avg 17}
               {:id 20 :cpu-avg 6}
               {:id 21 :cpu-avg 11}
               {:id 22 :cpu-avg 7}])

(declare best-solution)

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

(defn bench-comb
  [n]
  (println "benchmark partition " n " machines")
  (let [machines (take n machines)
        partitions #(comb/allocate-by-partitions 60 machines)]
    (bench-lazy-solutions partitions)))

(defn bench-logic
  [n]
  (println "benchmark logic solutions " n " machines")
  (let [machines (take n machines)
        partitions #(allocate-machines* machines 60)]
    (bench-lazy-solutions partitions)))

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

(defn bench-best-solution
  [n]
  (time
    (do
      (println "best solution for n: " n)
      (best-solution 60 (allocate-machines* (take n machines) 60)))))

(comment

  (count (combo/partitions (range 12))) ;4213597

  (for [n (range 20)]
    (do
      (println "n: " n)
      (bench-comb n)
      (bench-logic n)))

  (bench-logic 4)
  (bench-logic 5)
  (bench-logic 7)
  (bench-logic 8)
  (bench-best-solution 8)

  (bench-comb 8)
  (bench-comb 9)
  (bench-comb 12)

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
    (time (allocate-machines machines))))
