(ns server-cons.bench
  (:require [server-cons.core :refer [allocate-machines]]))

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

(comment

  (let [machines (take 1 machines)]
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
