(ns server-cons.core
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic])
  (:require [clojure.core.logic.fd :as fd]) )

(declare make-groups)

(defn- add-machines-into-group
  ([machines max-cpu group other-groups]
   (add-machines-into-group machines [] max-cpu max-cpu group other-groups))
  ([machines delayed-machines max-cpu remaining-cpu group other-groups]
   (conde
     ;; when no more machines and no delayed machines -> no more groups
     [(emptyo machines)
      (emptyo delayed-machines)
      (emptyo group)
      (emptyo other-groups)]

     ;; when no more machines but some delayed -> start over with the delayed machines
     [(emptyo machines)
      (!= delayed-machines [])
      (== group [])
      (make-groups delayed-machines max-cpu other-groups)]

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
                       (add-machines-into-group rest new-delayed-machines max-cpu remaining-cpu group other-groups))]))])))

(defn- make-groups
  ([machines max-cpu out]
   (conde
     ;; no more machines -> finish here
     [(== machines [])
      (== out [])]

     [(fresh [group rest-groups]
             ;; find the current group
             (conso group rest-groups out)

             (add-machines-into-group machines max-cpu group rest-groups)

             #_(all-groupedo machines out))]))
  )

;; implementation 2

(defn pairo [p]
  (fresh [a d]
    (== (lcons a d) p)))

(defn flatteno [level s out]
  (conde
    [(emptyo s) (== '() out)]
    [(pairo s)
     (conde
       [(== 0 level) (== '() out)]
       [(fd/< 0 level)
        (fresh [a d res-a res-d next-level]
               (fd/- level 1 next-level)
               (conso a d s)
               (flatteno next-level a res-a)
               (flatteno next-level d res-d)
               (appendo res-a res-d out))])]
    [(conso s '() out)]))

(defn all-groupedo
  [items groups]
  (fresh [flattened]
         #_(concateo groups flattened)
         (permuteo flattened items)))

(defn- make-groups2
  [machines max-cpu out]
  (all-groupedo machines out))

;; alternative 3
;;

(defn listo [l]
  (conde
    [(emptyo l) s#]
    [(pairo l)
     (fresh [d]
            (resto l d)
            (listo d))]))

(defn lolo
  "Relation where l is a list of lists"
  ([l]
   (lolo l nil))
  ([l g]
   (conde
     [(emptyo l)]
     [(fresh [head tail]
             (conso head tail l)
             (listo head)
             (trace-s)
             #_(when g
               (g head))
             (lolo tail g))])))

(defn containedo
  [subl l]
  (conda
    [(emptyo subl)]
    [(fresh [x rest-subl rest-l]
         (conso x rest-subl subl)
         (rembero x l rest-l)
         (containedo rest-subl rest-l))]))

(defn nonemptyo
  [l]
  (conda
    [(emptyo l) u#]
    [s#]))

(comment
  (run 15 [q]
       (lolo  q)
       )

  (run 15 [q]
       (lolo q
             nonemptyo)
       )

  (run 5 [q]
       (containedo [1 2 3] [1 2 3 4 5])
       ))

(defn allgroupscontainedo
  "A relation where every group in gs is contained in coll"
  [gs coll]
  (conde
    [(emptyo gs)]
    [(fresh [head tail]
            (conso head tail gs)
            (containedo head coll)
            (allgroupscontainedo tail coll))])
  )

(defn concateo
  [gs out]
  (all
    (lolo gs)
    (conde
      [(emptyo gs) (emptyo out)]
      [(fresh [head tail concated-tail]
              (conso head tail gs)
              (concateo tail concated-tail)
              (appendo head concated-tail out))])))

(defn partitiono
  "Relation where groups is a partition of machines"
  [machines groups]
  
    (lolo groups)
    (fresh [concated]
           (concateo groups concated)
           (permuteo machines concated))
  )

(comment

  (run 5 [q]
       (allgroupscontainedo q [1 2 3])
       )

  (run 5 [q]
       (concateo [[1] [2 3]] q)
       )

  (run 5 [q]
       (concateo q [1 2 3])
       )

  (run 15 [q]
       (partitiono [1 2 3] q)
       ))

(defn getcpuo
  [all-machines id cpu]
  (fresh [machine]
         (membero machine all-machines)
         (featurec machine {:id id :cpu-avg cpu})))

(defn sumcpuo
  [all-machines ids out]
  (conda
    [(emptyo ids) (== out 0)]
    [(fresh [id cpu rest rest-cpu]
           (conso id rest ids)
           (getcpuo all-machines id cpu)
           (fd/+ cpu rest-cpu out)
           (sumcpuo all-machines rest rest-cpu))]))

(comment

  (def machines [{:id 1 :cpu-avg 3} {:id 2 :cpu-avg 5} {:id 3 :cpu-avg 4}])

  (run 1 [q]
       (fresh [id]
              (== 1 id)
              (getcpuo machines id q)))

  (run 5 [q]
       (sumcpuo machines
                [1 2 3]
                q))

  (run 5 [q]
       (sumcpuo machines
                [1 3]
                q)))

(defn maxcpuo
  [all-machines max-cpu ids]
  (fresh [totalcpu]
         (sumcpuo all-machines ids totalcpu)
         (fd/>= max-cpu totalcpu)))

(comment

  (run 5 [q]
       (maxcpuo machines 11 [1 2 3]))
  (run 5 [q]
       (maxcpuo machines 12 [1 2 3]))

  (run 5 [q]
       (all
         (partitiono (map :id machines) q)
         (allmaxcpuo machines 11 q)))

  )

(defn allmaxcpuo
  [machines max-cpu groups]
  (conda
    [(emptyo groups)]
    [(fresh [head tail]
            (conso head tail groups)
            (maxcpuo machines max-cpu head)
            (allmaxcpuo machines max-cpu tail))]))

(defn make-groups3
  [machines max-cpu groups]
  (let [ids (map :id machines)]
    (fresh [concated]
           (lolo groups nonemptyo)
           (concateo groups concated)
           (permuteo concated ids)
           #_(distribute groups ::ff)
           #_(allmaxcpuo machines max-cpu groups) 
           )))

;; alternative 4
;;
(defn enoughcpuo
  [all-machines id max-cpu remaining-cpu]
  (fresh [cpu]
         (getcpuo all-machines id cpu)
         (fd/- max-cpu cpu remaining-cpu)
         (fd/>= remaining-cpu 0)))

(defn machinesgroupo
  ([all-machines machine-ids final-rest-ids min-id max-cpu group]
   (conda
     ;; no machines -> finish here
     [(emptyo machine-ids) (== machine-ids final-rest-ids) (emptyo group)]
     ;; no more cpu -> finish here
     [(== 0 max-cpu) (== machine-ids final-rest-ids) (emptyo group)]

     [(conde
        ;; branch 1: close group here
        [(== machine-ids final-rest-ids) (emptyo group)]

        ;; branch 2: try to add a machine to the group
        [(fresh [id rest-group rest-ids remaining-cpu]
                (rembero id machine-ids rest-ids)
                (fd/> id min-id)

                (enoughcpuo all-machines id max-cpu remaining-cpu)

                (conso id rest-group group)

                (machinesgroupo all-machines rest-ids final-rest-ids id remaining-cpu rest-group))])])))

(defn make-groups4
  ([all-machines machine-ids max-cpu groups]
   (make-groups4 all-machines 0 machine-ids max-cpu groups))

  ([all-machines min-id machine-ids max-cpu groups]
   (conda
     [(emptyo machine-ids) (emptyo groups)]
     [(fresh [group first-id rest-groups rest-ids permuted-ids next-ids]

             (machinesgroupo all-machines machine-ids rest-ids min-id max-cpu group)

             (!= group [])
             (conso group rest-groups groups)
             (firsto group first-id)
             (make-groups4 all-machines first-id rest-ids max-cpu rest-groups)
             )])))

(defn allocate-machines
  ([machines]
   (allocate-machines machines 60))
  ([machines max-cpu]
   (when (some #(> (:cpu-avg %) max-cpu) machines)
     (throw (Exception. "Some machines exceed max-cpu, no allocation possible")))

   (run* [q]
        (make-groups4 machines (map :id machines) max-cpu q))))



(comment


    (let [machines [{:id 1 :cpu-avg 22}
                    ]]
      (take 10 (allocate-machines machines 60)))

    (let [machines [{:id 1 :cpu-avg 22}
                    {:id 2 :cpu-avg 3}
                    ]]
      (take 10 (allocate-machines machines 60)))

    (let [machines [{:id 1 :cpu-avg 22}
                    {:id 2 :cpu-avg 17}
                    {:id 3 :cpu-avg 6}
                    {:id 4 :cpu-avg 17}
                    {:id 5 :cpu-avg 6}
                    {:id 6 :cpu-avg 11}
                    {:id 7 :cpu-avg 7}]]
      (take 20 (allocate-machines (take 6 machines))))
  )
