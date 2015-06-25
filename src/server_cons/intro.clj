(ns server-cons.intro
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic.fd :as fd]
            [clojure.core.logic.pldb :as pldb])
  (:use clojure.core.logic))

(run* [q]
      (== q true))

(run* [q]
      (== true q))

(run* [q]
      (== false q))

(run* [q]
      (== false q)
      (== true q))

; unbound vars -> solution can be anything
; [_0 _1]
(run* [q]
      (fresh [x y]
             (== q [x y])))

(run* [q]
      (fresh [x y]
             (== [x 1] [2 y])
             (== q [x y])))

(run* [q]
      (fresh [x y]
             (== q [x y])
             (== y 3)
             (== x 6)
             (membero y [3 4])  
             (membero x [1 2])))

; Disjunction

(run* [q]
      (conde
        [(== q 'tea)]
        [(== q 'coffee)]))

; relation that never ends
(defn nevero []
  (fresh []
         (nevero)))

; this will not terminate
(run* [q]
      (conde
        [(nevero)]
        [(== q 'tea)]))

; we found the 1 solution!
(run 1 [q]
      (conde
        [(nevero)]
        [(== q 'tea)]))

; we can't find 2, we never terminate
(run 2 [q]
      (conde
        [(nevero)]
        [(== q 'tea)]))

; that's minikanren: run* fresh conde unify

; extensions

; equations (FD: positive integers + 0)

(run* [q]
  (fresh [x y]
    (fd/in x y (fd/interval 0 9))
    (fd/eq
      (= (+ x y) 9) ; x + y = 9
      (= (+ (* x 2) (* y 4)) 24)) ; 2x + 4y = 24
    (== q [x y])))
; ([6 3])

(run* [q]
      (conso 'a q '(a b c))
      )

; no termination

(run* [q]
      (fresh [x]
             (== x [10 20 30])
             (membero q x)))

; sudoku

(defn pprint-sudoku [s]
  (->> s
       (partition 9)
       (map #(->> (partition 3 %)
                  (map (partial interpose " "))
                  (map clojure.string/join)
                  (clojure.string/join "  ")))
       (partition 3)
       (map (partial clojure.string/join "\n"))
       (clojure.string/join "\n\n")
       println)
  )
(pprint-sudoku [0 0 3  0 2 0  6 0 0
           9 0 0  3 0 5  0 0 1
           0 0 1  8 0 6  4 0 0

           0 0 8  1 0 2  9 0 0
           7 0 0  0 0 0  0 0 8
           0 0 6  7 0 8  2 0 0

           0 0 2  6 0 9  5 0 0
           8 0 0  2 0 3  0 0 9
           0 0 5  0 1 0  3 0 0])
(defn get-square [rows x y]
  (for [x (range x (+ x 3))
        y (range y (+ y 3))]
    (get-in rows [x y])))

(defn init-sudoku [vars hints]
  (if (seq vars)
    (let [hint (first hints)]
      (all
        (if-not (zero? hint)
          (== (first vars) hint)
          succeed)
        (init-sudoku (next vars) (next hints))))
    succeed))

(defn sudokufd [hints]
  (let [vars (repeatedly 81 lvar)
        rows (->> vars (partition 9) (map vec) (into []))
        cols (apply map vector rows)
        sqs  (for [x (range 0 9 3)
                   y (range 0 9 3)]
               (get-square rows x y))]
    (run 1 [q]
      (== q vars)
      (everyg #(fd/in % (fd/domain 1 2 3 4 5 6 7 8 9)) vars)
      (init-sudoku vars hints)
      (everyg fd/distinct rows)
      (everyg fd/distinct cols)
      (everyg fd/distinct sqs))))

(->> (sudokufd [0 0 3  0 2 0  6 0 0
                9 0 0  3 0 5  0 0 1
                0 0 1  8 0 6  4 0 0

                0 0 8  1 0 2  9 0 0
                7 0 0  0 0 0  0 0 8
                0 0 6  7 0 8  2 0 0

                0 0 2  6 0 9  5 0 0
                8 0 0  2 0 3  0 0 9
                0 0 5  0 1 0  3 0 0])
     first
     pprint-sudoku)




; in memory database: genealogy

(pldb/db-rel person p)
(pldb/db-rel parent x y)

(def genealogy
         (pldb/db
           [person 'Sonny]
           [person 'John]
           [person 'Bobby]
           [person 'Tommy]
           [person 'Tim]
           [person 'Michael]
           [person 'Alice]
           [person 'Ana]
           [person 'Nick]
           [person 'Mary]

           [parent 'John 'Bobby]
           [parent 'Alice 'Bobby]
           [parent 'Michael 'Tommy]
           [parent 'Michael 'Nick]
           [parent 'Ana 'Tim]
           [parent 'Mary 'John]
           [parent 'Sonny 'John]
           [parent 'Sonny 'Michael]))

(defn child [x y]
  (parent y x))

(defn sibling [x s]
  (fresh [p]
         (parent p x)
         (parent p s)
         (!= x s)))

(defn cousin [x c]
  (fresh [px pc]
         (parent px x)
         (parent pc c)
         (sibling px pc)))

(defn grandparent [gp x]
  (fresh [p]
         (child x p)
         (parent gp p)))

(pldb/with-db genealogy
         (run* [q]
           (child q 'John)))

(pldb/with-db genealogy
         (run* [q]
           (parent q 'John)))

(pldb/with-db genealogy
         (run* [q]
           (grandparent q 'Bobby)))

(pldb/with-db genealogy
         (run* [q]
           (sibling q 'John)))

(pldb/with-db genealogy
         (run* [q]
           (cousin q 'Bobby)))


; next: test.check
