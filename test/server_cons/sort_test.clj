(ns server-cons.sort-test
  (:require [clojure.test :refer [are deftest]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :refer [for-all]]))

; from http://www.philandstuff.com/slides/2014/euroclojure.html

(defn my-sort [coll] (seq (into (sorted-set) coll)))

(deftest my-sort-is-correct
  (are [x y] (= x y)
       (my-sort [1])         [1]
       (my-sort [1 2])       [1 2]
       (my-sort [2 1])       [1 2]
       (my-sort [:foo :bar]) [:bar :foo]
       (my-sort [1 4 5 3 2]) [1 2 3 4 5]))

; is this enough?

; The output is in ascending order
(defspec my-sort-output-increasing-prop
  (for-all [input (gen/vector gen/int)]
    (let [output (my-sort input)]
      (or (empty? output)
          (apply <= output)))))

; The output is a permutation of the input
(defspec my-sort-output-permutation-of-input
  (for-all [input (gen/vector gen/int)]
    (let [output (my-sort input)]
      (= (frequencies input) (frequencies output)))))

(comment
  (my-sort-output-increasing-prop)
  (my-sort-output-permutation-of-input)
  )

;{... :fail [[3 -2 -4 -2 6]], :shrunk {:smallest [[-2 -2]]} ...}
;     [3 -2 -4 -2 6]
;      [3 -2 -4 -2]
;    [3 -2 -4] [3 -2 -2]
;       [3 -2] [-2 -2]
;            [-2]
