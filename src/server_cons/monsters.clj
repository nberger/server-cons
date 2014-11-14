(ns server-cons.monsters
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.fd :as fd]))

(defn allocate-monsterso [points monsters out]
  "This relation finds all the allocations of monsters for the given points.
  "
  (l/conde
   ;; if we have no points left, then we get an empty list
   [(fd/<= points 0)
    (l/== out [])]

   [(l/fresh [head
              cost
              tail
              remaining-points
              rest]

             ;; find the current monster and the tail list.
             (l/conso [head cost] tail monsters)

             (l/conde
              ;; branch one: add one of the current monsters to the list
              ;; subtracting its cost from the points
              [(fd/- points cost remaining-points)
               (fd/<= 0 remaining-points)
               (allocate-monsterso remaining-points monsters rest)
               (l/conso head rest out)]

              ;; branch two: move on to the next monster in the list
              [(allocate-monsterso points tail out)]

              ;; branch three: skip the next monster in the list
              [(l/fresh [_ ttail]
                (l/conso _ ttail tail)
                (allocate-monsterso points ttail out))]))]))


 (l/run 10 [q]
   (allocate-monsterso 3
                       [[:a 1] [:b 2] [:c 1]]
                       q))
