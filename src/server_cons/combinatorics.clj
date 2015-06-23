(ns server-cons.combinatorics
  (:require [clojure.math.combinatorics :as combo]))

(defn enoughcpu
  [max-cpu [machine & more]]
  (if machine
    (let [cpu (:cpu-avg machine)]
      (and
        (<= cpu max-cpu)
        (enoughcpu (- max-cpu cpu) more)))
    true))

(defn all-groups-enough-cpu?
  [max-cpu groups]
  (every? (partial enoughcpu max-cpu) groups)
  )

(defn allocate-by-partitions
  [max-cpu machines]
  (->> machines
       (combo/partitions)
       (filter (partial all-groups-enough-cpu? max-cpu))))
