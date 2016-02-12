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
                                 (. pq (peek))) ))))
