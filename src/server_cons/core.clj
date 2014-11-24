(ns server-cons.core
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic])
  (:require [clojure.core.logic.fd :as fd]) )

(defn getcpuo
  [all-machines id cpu]
  (fresh [machine]
         (membero machine all-machines)
         (featurec machine {:id id :cpu-avg cpu})))

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
     [(fresh [group first-id rest-groups rest-ids]

             (machinesgroupo all-machines machine-ids rest-ids min-id max-cpu group)

             (!= group [])
             (conso group rest-groups groups)
             (firsto group first-id)
             (make-groups4 all-machines first-id rest-ids max-cpu rest-groups)
             )])))

(defn- find-by-id
  [items id]
  (->> items
       (filter #(= (:id %) id))
       first))

(defn ids->machines
  [machines ids]
  (map #(find-by-id machines %) ids))

(defn allocate-machines
  ([machines]
   (allocate-machines machines 60))
  ([machines max-cpu]
   (when (some #(> (:cpu-avg %) max-cpu) machines)
     (throw (Exception. "Some machines exceed max-cpu, no allocation possible")))

   (->>
     (run* [q]
          (make-groups4 machines (map :id machines) max-cpu q))
     (map (partial map (partial ids->machines machines))))))
