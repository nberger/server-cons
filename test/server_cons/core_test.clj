(ns server-cons.core-test
  (:require [server-cons.core :refer [allocate-machines]]
            [server-cons.generators :refer [machines-gen]]
            [midje.sweet :refer [=> throws fact facts future-fact]]
            [clojure.core.logic :refer [run]]
            [clojure.test.check.clojure-test :as ct :refer (defspec)]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(def prop-all-machines-are-allocated
  (prop/for-all [[machines max-cpu] machines-gen]
    (let [grouped-machines (allocate-machines machines max-cpu)
          flattened (apply concat grouped-machines)]
      (and
        (= (set flattened) (set machines))
        (= (count flattened) (count machines))))))

(def prop-no-group-exceeds-max-cpu
  (prop/for-all [[machines max-cpu] machines-gen]
    (let [grouped-machines (allocate-machines machines max-cpu)]
      (every? #(>= max-cpu (reduce + (map :cpu-avg %))) grouped-machines))))


(defspec all-machines-are-allocated 20 prop-all-machines-are-allocated :max-size 80)

(defspec no-group-exceeds-max-cpu 20 prop-no-group-exceeds-max-cpu :max-size 80)

(facts "about server-cons"

  (fact
    (let [machines []]
      (allocate-machines machines)) => [])

  (fact
    (let [machines [{:id 1 :cpu-avg 22}]]
      (allocate-machines machines)) => `(({:id 1 :cpu-avg 22})))

  (fact
    (let [machines [{:id 1 :cpu-avg 22}
                    {:id 2 :cpu-avg 17}]]
      (allocate-machines machines) => `(({:id 1 :cpu-avg 22}
                                         {:id 2 :cpu-avg 17}))))

  (fact
    (let [machines [{:id 1 :cpu-avg 22}
                    {:id 2 :cpu-avg 17}
                    {:id 3 :cpu-avg 22}
                    {:id 4 :cpu-avg 3}
                    {:id 5 :cpu-avg 6}
                    {:id 6 :cpu-avg 11}
                    {:id 7 :cpu-avg 7}]]
      (allocate-machines machines))
         => `(({:id 1 :cpu-avg 22}
                {:id 2 :cpu-avg 17}
                {:id 4 :cpu-avg 3}
                {:id 5 :cpu-avg 6}
                {:id 6 :cpu-avg 11})
               ({:id 7 :cpu-avg 7}
                {:id 3 :cpu-avg 22})))

  (fact

    (let [machines [{:id 1 :cpu-avg 22}
                    {:id 2 :cpu-avg 17}
                    {:id 3 :cpu-avg 3}
                    {:id 4 :cpu-avg 3}
                    {:id 5 :cpu-avg 6}
                    {:id 6 :cpu-avg 11}
                    {:id 7 :cpu-avg 35}
                    {:id 8 :cpu-avg 3}
                    {:id 9 :cpu-avg 6}
                    {:id 10 :cpu-avg 11}
                    {:id 11 :cpu-avg 35}
                    {:id 12 :cpu-avg 26}
                    {:id 13 :cpu-avg 29}
                    {:id 14 :cpu-avg 22}
                    ]]
      (allocate-machines machines))

    (let [machines [{:id 1 :cpu-avg 22}
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
                    {:id 74 :cpu-avg 7}]]
      (time (allocate-machines machines)))
         => `(({:cpu-avg 22, :id 1}
                {:cpu-avg 17, :id 2}
                {:cpu-avg 3, :id 4}
                {:cpu-avg 6, :id 5}
                {:cpu-avg 11, :id 6})
               ({:cpu-avg 7, :id 10}
                {:cpu-avg 29, :id 9}
                {:cpu-avg 22, :id 3})
               ({:cpu-avg 35, :id 7})
               ({:cpu-avg 26, :id 8})))



  )
