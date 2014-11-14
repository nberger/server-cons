(ns server-cons.core
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic])
  (:require [clojure.core.logic.fd :as fd]) )

(def max-cpu 60)

(defn allocate-machines
  ([machines out]
   (allocate-machines machines max-cpu out))
  ([machines remaining-cpu out]
   (conde
     ;; no more machines -> finish here
     [(== machines [])
      ]

     [(fresh [group rest-groups machine rest machine-cpu-avg cpu]
             ;; find the current group
             (conso group rest-groups out)

             ;; find the machine & rest
             (conso machine rest machines)

             ;; extract this machine cpu
             (featurec machine {:cpu-avg machine-cpu-avg})

             ;; ensure this machine does not exceed max cpu
             (fd/<= machine-cpu-avg max-cpu)

             ;; resulting cpu after discounting current machine cpu
             (fd/- remaining-cpu machine-cpu-avg cpu)

             (conde
               ;; branch one: There's room in current group for this machine, add it
               [(fresh [_]
                       (fd/<= 0 cpu)
                       (allocate-machines rest cpu out)
                       (conso machine _ group))
                ]

               ;; branch two: no room -> create another group and continue with the rest
               #_[(fresh [_]
                       (fd/- remaining-cpu machine-cpu-avg cpu)
                       (fd/> 0 cpu)
                       (allocate-machines rest max-cpu rest-groups)
                       (conso [machine] _ rest-groups))]
               ))]))

  )

(comment

  (let [machines [{:id 1 :cpu-avg 22}
                  ]]
    (run 10 [q]
         (allocate-machines machines
                            q)))

  (let [machines [{:id 1 :cpu-avg 22}
                  {:id 2 :cpu-avg 17}
                  ]]
    (run 10 [q]
         (allocate-machines machines
                            q)))

  (let [machines [{:id 1 :cpu-avg 22}
                  {:id 2 :cpu-avg 17}
                  {:id 3 :cpu-avg 21}
                  {:id 4 :cpu-avg 3}
                  {:id 5 :cpu-avg 6}
                  {:id 6 :cpu-avg 11}
                  {:id 7 :cpu-avg 7}]]
    (run 10 [q]
         (allocate-machines machines
                            q)))

  (let [machines [{:id 1 :cpu-avg 0.2}
               {:id 2 :cpu-avg 0.17}
               {:id 3 :cpu-avg 0.02}
               {:id 4 :cpu-avg 0.03}
               {:id 5 :cpu-avg 0.6}
               {:id 6 :cpu-avg 0.11}
               {:id 7 :cpu-avg 0.7}]
        new-machines-count 2
        survival-machines (repeatedly new-machines-count lvar)
        machines (repeatedly (count current-machines) lvar)
        machine-assignments (repeatedly (count current-machines) lvar)
        machines-with-assignments (map list machines machine-assignments)
        resulting-machines [survival-machines machines-with-assignments]
        ]
    (run 1 [q]
         (== q resulting-machines)
         (== machines current-machines)
         (fresh [x]
                (membero x machine-assignments)
                (membero x survival-machines))
         (everyg fd/distinct survival-machines)
         )
    )

  #_"((1 3) (1 2 3 4 5 6 7) (1 1 3 3 1 3 3))"

  (cond
      false true)
  )
