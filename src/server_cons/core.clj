(ns server-cons.core
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic])
  (:require [clojure.core.logic.fd :as fd]) )

(defn add-machines-into-group
  ([machines max-cpu group other-groups]
   (add-machines-into-group machines [] max-cpu max-cpu group other-groups))
  ([machines delayed-machines max-cpu remaining-cpu group other-groups]
   (conde
     ;; when no more machines and no delayed machines -> stop here
     [(== machines [])
      (== delayed-machines [])
      (== group [])
      (== other-groups [])]

     ;; when no more machines but some delayed -> start over with the delayed machines
     [(== machines [])
      (!= delayed-machines [])
      (== group [])
      (allocate-machines delayed-machines max-cpu other-groups)]

     [(fresh [machine rest machine-cpu-avg]
             ;; get the machine & rest
             (conso machine rest machines)

             ;; extract this machine cpu
             (featurec machine {:cpu-avg machine-cpu-avg})

             (conde

               ;; branch one: There's room in current group for this machine -> add it
               [(fresh [next-cpu group-rest]
                       ;; resulting cpu after discounting current machine cpu
                       (fd/- remaining-cpu machine-cpu-avg next-cpu)
                       (fd/<= 0 next-cpu)
                       (add-machines-into-group rest delayed-machines max-cpu next-cpu group-rest other-groups)
                       (conso machine group-rest group))]

               ;; branch two: delay this machine and continue with rest
               [(fresh [new-delayed-machines]
                       (conso machine delayed-machines new-delayed-machines)
                       (add-machines-into-group rest new-delayed-machines max-cpu remaining-cpu group other-groups))]

               ;; branch three: delay this and the next machine and continue with the rest
               [(fresh [new-delayed-machines]
                       (conso machine delayed-machines new-delayed-machines)
                       (add-machines-into-group rest new-delayed-machines max-cpu remaining-cpu group other-groups))]
                ))])))

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

  (let [machines []]
    (run 1 [q] (allocate-machines machines q)))
  (let [machines [{:id 1 :cpu-avg 22}]]
    (run 1 [q] (allocate-machines machines q)))

  (let [machines [{:id 1 :cpu-avg 22}
                  {:id 2 :cpu-avg 17}
                  ]]
    (run 1 [q] (allocate-machines machines q)))
  ;; ((({:cpu-avg 22, :id 1} {:cpu-avg 17, :id 2})))

  (let [machines [{:id 1 :cpu-avg 22}
                  {:id 2 :cpu-avg 17}
                  {:id 3 :cpu-avg 22}
                  {:id 4 :cpu-avg 3}
                  {:id 5 :cpu-avg 6}
                  {:id 6 :cpu-avg 11}
                  {:id 7 :cpu-avg 7}]]
    (run 1 [q] (allocate-machines machines q)))
  ;; ((({:cpu-avg 22, :id 1} {:cpu-avg 17, :id 2} {:cpu-avg 3, :id 4} {:cpu-avg 6, :id 5} {:cpu-avg 11, :id 6}) ({:cpu-avg 7, :id 7} {:cpu-avg 22, :id 3})))

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
    (run 1 [q] (allocate-machines machines q)))
  ;; ((({:cpu-avg 22, :id 1} {:cpu-avg 17, :id 2} {:cpu-avg 3, :id 4} {:cpu-avg 6, :id 5} {:cpu-avg 11, :id 6}) ({:cpu-avg 7, :id 10} {:cpu-avg 29, :id 9} {:cpu-avg 22, :id 3}) ({:cpu-avg 35, :id 7}) ({:cpu-avg 26, :id 8})))


  )
