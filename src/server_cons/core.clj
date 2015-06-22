(ns server-cons.core
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic])
  (:require [clojure.core.logic.fd :as fd]) )

(defn getcpuo
  [all-machines id cpu]
  (fresh [machine]
         (membero machine all-machines)
         (featurec machine {:id id :cpu-avg cpu})))

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

     [(conda
        ;; branch 1: try to add a machine to the group
        [(fresh [id rest-group rest-ids remaining-cpu]
                (rembero id machine-ids rest-ids)
                (fd/> id min-id)

                (enoughcpuo all-machines id max-cpu remaining-cpu)

                (conso id rest-group group)

                (machinesgroupo all-machines rest-ids final-rest-ids id remaining-cpu rest-group))]

        ;; branch 2: close group here
        [(== machine-ids final-rest-ids) (emptyo group)])])))

(defn make-groups
  ([all-machines machine-ids max-cpu groups]
   (make-groups all-machines 0 machine-ids max-cpu groups))

  ([all-machines min-id machine-ids max-cpu groups]
   (conda
     [(emptyo machine-ids) (emptyo groups)]
     [(fresh [group first-id rest-groups rest-ids]

             (machinesgroupo all-machines machine-ids rest-ids min-id max-cpu group)

             (!= group [])
             (conso group rest-groups groups)
             (firsto group first-id)
             (make-groups all-machines first-id rest-ids max-cpu rest-groups)
             )])))

(defn ids-partition->machines-partition
  [all-machines ids-partition]
  (let [machines-index (->> all-machines
                          (map (juxt :id identity))
                          (into {}))]
    (map (partial map machines-index) ids-partition)))

(defn allocate-machines*
  ([machines max-cpu]
   (when (some #(> (:cpu-avg %) max-cpu) machines)
     (throw (Exception. "Some machines exceed max-cpu, no allocation possible")))

   (let [ids (map :id machines)]
     (when-not (= (count ids) (count (set ids)))
       (throw (Exception. "Some machine ids are repeated")))

     (->>
       (run* [q]
             (make-groups machines ids max-cpu q))
       (map (partial ids-partition->machines-partition machines))))))

(defn allocate-machines
  ([machines]
   (allocate-machines machines 60))
  ([machines max-cpu]
   (first (allocate-machines* machines max-cpu))))
