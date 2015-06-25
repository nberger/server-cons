(ns server-cons.core-test
  (:require [server-cons.core :refer [allocate-machines allocate-machines*]]
            [server-cons.generators :refer [machines-gen]]
            [clojure.core.logic :refer [run]]
            [clojure.test :refer [is testing deftest]]
            [clojure.test.check.clojure-test :as ct :refer (defspec)]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(def prop-all-machines-are-allocated
  (prop/for-all [[machines max-cpu] machines-gen]
    (let [result (apply concat (allocate-machines machines max-cpu))]
      (and
        (= (set result) (set machines))
        (= (count result) (count machines))))))

(def prop-no-group-exceeds-max-cpu
  (prop/for-all [[machines max-cpu] machines-gen]
    (let [grouped-machines (allocate-machines machines max-cpu)]
      (every? #(>= max-cpu (reduce + (map :cpu-avg %)))
              grouped-machines))))

(defspec
  all-machines-are-allocated
  {:num-tests 20
   :max-size 10}
  prop-all-machines-are-allocated)

(defspec
  no-group-exceeds-max-cpu
  {:num-tests 20
   :max-size 10}
  prop-no-group-exceeds-max-cpu)

(deftest allocate-machines-test

   (testing "no machines"
    (let [machines []]
       (is (= [] (allocate-machines machines)))))

  (testing "1 machine"
    (let [machines [{:id 1 :cpu-avg 22}]]
      (is (= [[{:id 1 :cpu-avg 22}]]
             (allocate-machines machines)))))


  (testing "2 machines"
    (let [machines [{:id 1 :cpu-avg 22}
                    {:id 2 :cpu-avg 17}]]
      (is (= [[{:id 1 :cpu-avg 22}
               {:id 2 :cpu-avg 17}]]
             (allocate-machines machines)))))

  (testing "7 machines"
    (let [machines [{:id 1 :cpu-avg 22}
                    {:id 2 :cpu-avg 17}
                    {:id 3 :cpu-avg 22}
                    {:id 4 :cpu-avg 3}
                    {:id 5 :cpu-avg 6}
                    {:id 6 :cpu-avg 11}
                    {:id 7 :cpu-avg 7}]
          result (allocate-machines* machines 60)]
      (is (< 10 (count result)))
      (is (contains? (set result)
                     [[{:id 1 :cpu-avg 22}
                       {:id 2 :cpu-avg 17}
                       {:id 4 :cpu-avg 3}
                       {:id 5 :cpu-avg 6}
                       {:id 6 :cpu-avg 11}]
                      [{:id 3 :cpu-avg 22}
                       {:id 7 :cpu-avg 7}]]
                     ))))

  (testing "10 machines"

    (let [machines [{:id 1 :cpu-avg 22}
                    {:id 2 :cpu-avg 17}
                    {:id 3 :cpu-avg 22}
                    {:id 4 :cpu-avg 3}
                    {:id 5 :cpu-avg 6}
                    {:id 6 :cpu-avg 11}
                    {:id 7 :cpu-avg 35}
                    {:id 8 :cpu-avg 26}
                    {:id 9 :cpu-avg 29}
                    {:id 10 :cpu-avg 7}]]
      (is (->> (allocate-machines* machines 60)
               (filter (partial = [[{:cpu-avg 22, :id 1}
                                    {:cpu-avg 17, :id 2}
                                    {:cpu-avg 3, :id 4}
                                    {:cpu-avg 6, :id 5}
                                    {:cpu-avg 11, :id 6}]
                                   [{:cpu-avg 22, :id 3}
                                    {:cpu-avg 29, :id 9}
                                    {:cpu-avg 7, :id 10}]
                                   [{:cpu-avg 35, :id 7}]
                                   [{:cpu-avg 26, :id 8}]]))
               first
               boolean)))))
