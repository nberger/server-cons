(ns server-cons.generators
  (:require [clojure.test.check.generators :as gen]))

(defn machine-gen
  [max-cpu]
  (gen/hash-map :cpu-avg (gen/choose 1 max-cpu)))

(def max-cpu-gen (gen/choose 1 100))

(def machines-gen
  (gen/bind max-cpu-gen
            (fn [max-cpu] (gen/tuple
                            (gen/vector (machine-gen max-cpu))
                            (gen/return max-cpu)))))
