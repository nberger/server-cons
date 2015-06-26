(ns server-cons.helpers)

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
       println))
