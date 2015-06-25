(ns server-cons.combinatorics
  (:require [clojure.math.combinatorics :as combo]))

(defn enoughcpu?
  [max-cpu machines]
  (>= max-cpu (reduce + :cpu-avg machines)))

(defn all-groups-enough-cpu?
  [max-cpu groups]
  (every? (partial enoughcpu? max-cpu) groups))

(defn allocate-by-partitions
  [max-cpu machines]
  (->> machines
       (combo/partitions)
       (filter (partial all-groups-enough-cpu? max-cpu))))
