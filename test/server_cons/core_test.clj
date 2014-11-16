(ns server-cons.core-test
  (:require [midje.sweet :refer [=> throws fact facts future-fact]]
            [clojure.core.logic :refer [run]]))

(require '[server-cons.core :refer [allocate-machines]])


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
                    {:id 3 :cpu-avg 22}
                    {:id 4 :cpu-avg 3}
                    {:id 5 :cpu-avg 6}
                    {:id 6 :cpu-avg 11}
                    {:id 7 :cpu-avg 35}
                    {:id 8 :cpu-avg 26}
                    {:id 9 :cpu-avg 29}
                    {:id 10 :cpu-avg 7}]]
      (allocate-machines machines))
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
