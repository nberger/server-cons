(ns server-cons.core
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic])
  (:require [clojure.core.logic.fd :as fd]) )

(defn add-machines-into-group
  ([machines max-cpu group other-groups]
   (add-machines-into-group machines max-cpu max-cpu group other-groups))
  ([machines max-cpu remaining-cpu group other-groups]
   (conde
     ;; stop when no more machines
     [(== machines [])
      (== group [])
      (== other-groups [])]

     [(fresh [machine rest machine-cpu-avg next-cpu]
             ;; find the machine & rest
             (conso machine rest machines)

             ;; extract this machine cpu
             (featurec machine {:cpu-avg machine-cpu-avg})

             ;; resulting cpu after discounting current machine cpu
             (fd/- remaining-cpu machine-cpu-avg next-cpu)

             (conde
               ;; branch one: There's room in current group for this machine, add it
               [(fresh [group-rest]
                       (fd/<= 0 next-cpu)
                       (add-machines-into-group rest max-cpu next-cpu group-rest other-groups)
                       (conso machine group-rest group))]

               ;; no more room, continue with other groups
               [(fd/> 0 next-cpu)
                (== group [])
                (allocate-machines machines max-cpu other-groups)]))])))

(defn allocate-machines
  ([machines out]
   (allocate-machines machines 60 out))
  ([machines max-cpu out]
   (conde
     ;; no more machines -> finish here
     [(== machines [])
      (== out [])]

     [(fresh [group rest-groups]
             ;; find the current group
             (conso group rest-groups out)

             (add-machines-into-group machines max-cpu group rest-groups)
             )]))

  )

(comment

  (let [machines [
                  ]]
    (run 10 [q]
         (allocate-machines machines
                            q)))

  (let [machines [{:id 1 :cpu-avg 22}]]
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
                  {:id 3 :cpu-avg 22}
                  {:id 4 :cpu-avg 3}
                  {:id 5 :cpu-avg 6}
                  {:id 6 :cpu-avg 11}
                  {:id 7 :cpu-avg 7}]]
    (run 10 [q]
         (allocate-machines machines q)))

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
