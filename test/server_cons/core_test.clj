(ns server-cons.core-test
  (:require [server-cons.core :refer [allocate-machines]]
            [clojure.test :refer [is deftest testing]]
            [clojure.core.logic :refer [run]]
            [clojure.test.check.clojure-test :as ct :refer (defspec)]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(defn machine-gen
  [max-cpu]
  (gen/hash-map :cpu-avg (gen/choose 1 max-cpu)))

(def machines-gen
  (gen/bind (gen/choose 1 100)
            #(gen/tuple (gen/vector (machine-gen %)) (gen/return %))))

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


(defspec
  all-machines-are-allocated
  {:num-tests 20
   :max-size 10}
  prop-all-machines-are-allocated )

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
                    {:id 7 :cpu-avg 7}]]
      (is (= [[{:id 1 :cpu-avg 22}
               {:id 2 :cpu-avg 17}
               {:id 4 :cpu-avg 3}
               {:id 5 :cpu-avg 6}
               {:id 6 :cpu-avg 11}]
              [{:id 7 :cpu-avg 7}
               {:id 3 :cpu-avg 22}]]
             (allocate-machines machines)))))

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
      (is (= [[{:cpu-avg 22, :id 1}
               {:cpu-avg 17, :id 2}
               {:cpu-avg 3, :id 4}
               {:cpu-avg 6, :id 5}
               {:cpu-avg 11, :id 6}]
               [{:cpu-avg 7, :id 10}
                {:cpu-avg 29, :id 9}
                {:cpu-avg 22, :id 3}]
               [{:cpu-avg 35, :id 7}]
               [{:cpu-avg 26, :id 8}]]
             (allocate-machines machines))))))
