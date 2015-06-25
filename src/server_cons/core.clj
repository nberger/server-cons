(ns server-cons.core
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic])
  (:require [clojure.core.logic.fd :as fd]))

(defn getcpuo
  [all-machines id cpu]
  (fresh [machine]
         (featurec machine {:cpu-avg cpu})
         (membero [id machine] all-machines)))

(defn enoughcpuo
  [all-machines id max-cpu remaining-cpu]
  (fresh [cpu]
         (getcpuo all-machines id cpu)
         (fd/- max-cpu cpu remaining-cpu)
         (fd/>= remaining-cpu 0))) ; ## without this we might exceed max-cpu in each group

(defn machinesgroupo
  [all-machines machine-ids final-rest-ids min-id max-cpu group]
  (conda
    ;; no machines -> finish here
    [(emptyo machine-ids) (== machine-ids final-rest-ids) (emptyo group)]
    ;; no more cpu -> finish here
    [(== 0 max-cpu) (== machine-ids final-rest-ids) (emptyo group)] ; ## optimization: without this we might continue trying to put machines into a full group

    [(conda ; ## if using conde instead of conda, we'd get both the smaller and the bigger group

       ;; branch 1: try to add a machine to the group
       [(fresh [id rest-group rest-ids remaining-cpu]
               (rembero id machine-ids rest-ids)
               (fd/>= id min-id) ; ## without this constraint we'd get all the permutations of each group
               (enoughcpuo all-machines id max-cpu remaining-cpu)
               (conso id rest-group group)
               (machinesgroupo all-machines rest-ids final-rest-ids id remaining-cpu rest-group))]

       ;; branch 2: close group here
       [(== machine-ids final-rest-ids) (emptyo group)])]))

(defn machines-partitiono
  ([all-machines machine-ids max-cpu groups]
   (machines-partitiono all-machines 0 machine-ids max-cpu groups))

  ([all-machines min-id machine-ids max-cpu groups]
   (conda
     [(emptyo machine-ids) (emptyo groups)]
     [(fresh [group first-id rest-groups rest-ids]
             (machinesgroupo all-machines machine-ids rest-ids min-id max-cpu group)

             (conso group rest-groups groups)
             (firsto group first-id) ; extract the first-id, to ensure the next group starts after it
             ; let's continue with the next group, passing the rest-ids (machines not grouped yet)
             (machines-partitiono all-machines first-id rest-ids max-cpu rest-groups))])))

(defn ids->machines
  [all-machines ids-partition]
  (map (comp second all-machines) ids-partition))

(defn allocate-machines*
  ([machines max-cpu]
   (when (some #(> (:cpu-avg %) max-cpu) machines)
     (throw (Exception. "Some machines exceed max-cpu, no allocation possible")))

   (let [machines (vec (map-indexed vector machines))
         ids (mapv first machines)]
     (->>
       (run* [q]
             (machines-partitiono machines ids max-cpu q))
       (map (partial map (partial ids->machines machines)))))))

(defn allocate-machines
  ([machines]
   (allocate-machines machines 60))
  ([machines max-cpu]
   (first (allocate-machines* machines max-cpu))))

(comment

  (allocate-machines* [{:cpu-avg 20}] 60)
  (allocate-machines* [{:cpu-avg 20} {:cpu-avg 35}] 60)
  (allocate-machines* [{:cpu-avg 20} {:cpu-avg 45}] 60)
  (allocate-machines* [{:cpu-avg 20} {:cpu-avg 45} {:cpu-avg 36} {:cpu-avg 1} {:cpu-avg 2}] 45)


  (def machine-groups->name-groups (partial map (partial map :name)))
  (def machine-groups->cpu-groups (partial map (partial map :cpu-avg)))

  (->> (allocate-machines* [{:cpu-avg 20 :name "m1"} {:cpu-avg 35 :name "m2"} {:cpu-avg 20 :name "m3"}] 60)
       (map
         #_identity
         machine-groups->cpu-groups
         #_machine-groups->name-groups))

  )
