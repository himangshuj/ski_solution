(ns ski-solution.core-test
  (:require [clojure.test :refer :all]
            [ski-solution.core :refer :all])
  (:import [java.util PriorityQueue Collections]))

(def vals (map
            map->SkiGrid
            [{:start-val 4, :val 4, :hops 0, :column 0, :propagated false, :row 0}
             {:start-val 9, :val 9, :hops 1, :column 2, :propagated false, :row 1}
             {:start-val 3, :val 3, :hops 2, :column 1, :propagated false, :row 2}
             {:start-val 5, :val 5, :hops 0, :column 3, :propagated false, :row 2}
             {:start-val 5, :val 5, :hops 0, :column 1, :propagated false, :row 1}
             {:start-val 6, :val 6, :hops 0, :column 0, :propagated false, :row 2}
             {:start-val 4, :val 4, :hops 0, :column 0, :propagated false, :row 3}
             {:start-val 8, :val 6, :hops 0, :column 3, :propagated false, :row 3}]))

(deftest comparision-test
  (testing "comparision tests"
    (let [pq          (PriorityQueue. node-comparator)
          _           (. pq (addAll vals))
          vals-sorted (sort node-comparator vals)]
      (are [x y] (= x y)
                 (first vals-sorted) (map->SkiGrid {:start-val 9, :val 9, :hops 1, :column 2, :propagated false, :row 1})
                 (first vals-sorted) (. pq (poll))
                 (second vals-sorted) (. pq (poll))
                 (. pq (peek)) (do
                                 (doto pq
                                   (.add (map->SkiGrid {:start-val 3, :val 3, :hops 2, :column 1, :propagated false, :row 2})))
                                 (. pq (peek))))))
  (testing "comparison chaos"
    (let [queue-list  [(map->SkiGrid {:val 7, :start-val 9, :row 0, :column 2, :hops 2, :propagated false}),
                       (map->SkiGrid {:val 6, :start-val 6, :row 3, :column 3, :hops 1, :propagated false}),
                       (map->SkiGrid {:val 8, :start-val 8, :row 0, :column 1, :hops 1, :propagated false}),
                       (map->SkiGrid {:val 5, :start-val 5, :row 2, :column 3, :hops 1, :propagated false}),
                       (map->SkiGrid {:val 4, :start-val 4, :row 3, :column 0, :hops 1, :propagated false}),
                       (map->SkiGrid {:val 6, :start-val 6, :row 2, :column 0, :hops 1, :propagated false}),
                       (map->SkiGrid {:val 5, :start-val 9, :row 1, :column 1, :hops 2, :propagated false}),
                       (map->SkiGrid {:val 4, :start-val 4, :row 0, :column 0, :hops 1, :propagated false}),
                       (map->SkiGrid {:val 1, :start-val 1, :row 3, :column 2, :hops 1, :propagated false}),
                       (map->SkiGrid {:val 2, :start-val 2, :row 1, :column 0, :hops 1, :propagated false}),
                       (map->SkiGrid {:val 3, :start-val 3, :row 0, :column 3, :hops 1, :propagated false}),
                       (map->SkiGrid {:val 3, :start-val 3, :row 2, :column 1, :hops 1, :propagated false}),
                       (map->SkiGrid {:val 2, :start-val 9, :row 2, :column 2, :hops 2, :propagated false}),
                       (map->SkiGrid {:val 4, :start-val 4, :row 3, :column 1, :hops 1, :propagated false}),
                       (map->SkiGrid {:val 3, :start-val 9, :row 1, :column 3, :hops 2, :propagated false})]
          pq          (PriorityQueue. node-comparator)

          _           (. pq (addAll queue-list))
          queue-list# (sort node-comparator queue-list)]
      (are [x y] (= (select-keys x [:val :start-val :hops])
                    (select-keys y [:val :start-val :hops]))
                 (nth queue-list# 0) (. pq (poll))
                 (nth queue-list# 1) (. pq (poll))
                 (nth queue-list# 2) (. pq (poll))
                 (nth queue-list# 3) (. pq (poll))
                 (nth queue-list# 4) (. pq (poll))
                 (nth queue-list# 5) (. pq (poll))
                 (nth queue-list# 6) (. pq (poll))
                 (nth queue-list# 7) (. pq (poll))
                 (nth queue-list# 8) (. pq (poll))
                 (nth queue-list# 9) (. pq (poll))
                 (nth queue-list# 10) (. pq (poll))
                 (nth queue-list# 11) (. pq (poll))
                 (nth queue-list# 12) (. pq (poll))))))
