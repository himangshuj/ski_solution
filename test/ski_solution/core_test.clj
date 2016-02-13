(ns ski-solution.core-test
  (:require [clojure.test :refer :all]
            [ski-solution.core :refer :all])
  (:import [java.util PriorityQueue]
           [ski_solution.util FibonacciHeap])
  (:refer-clojure :exclude [peek]))

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
      (are [x y] (= (select-keys x [:val :hops])
                    (select-keys y [:val :hops]))
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
      (are [x y] (= (select-keys x [:val :hops])
                    (select-keys y [:val :hops]))
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
                 (nth queue-list# 12) (. pq (poll)))))

  (testing "fibonanni"
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
          queue-list# (sort node-comparator queue-list)
          fb          (FibonacciHeap.)
          _           (doseq [node queue-list]
                        (let []
                          (. fb (enqueue node (get-fibonacci-key node)))))]
      (are [x y] (= (select-keys x [:val :hops :start-val])
                    (select-keys (. y (getValue) )  [:val :hops :start-val]))
                 (nth queue-list# 0) (. fb (dequeueMin))
                 (nth queue-list# 1) (. fb (dequeueMin))
                 (nth queue-list# 2) (. fb (dequeueMin))
                 (nth queue-list# 3) (. fb (dequeueMin))
                 (nth queue-list# 4) (. fb (dequeueMin))
                 (nth queue-list# 5) (. fb (dequeueMin))
                 (nth queue-list# 6) (. fb (dequeueMin))
                 (nth queue-list# 7) (. fb (dequeueMin))
                 (nth queue-list# 8) (. fb (dequeueMin))
                 (nth queue-list# 9) (. fb (dequeueMin))
                 (nth queue-list# 10) (. fb (dequeueMin))
                 (nth queue-list# 11) (. fb (dequeueMin))
                 (nth queue-list# 12) (. fb (dequeueMin)))))

  (testing "fibonacci decrease key test"
    (let [queue-list  [(map->SkiGrid {:val 7, :start-val 7, :row 0, :column 2, :hops 1, :propagated false}),
                       (map->SkiGrid {:val 6, :start-val 6, :row 3, :column 3, :hops 1, :propagated false}),
                       (map->SkiGrid {:val 8, :start-val 8, :row 0, :column 1, :hops 1, :propagated false}),
                       (map->SkiGrid {:val 5, :start-val 5, :row 2, :column 3, :hops 1, :propagated false}),
                       (map->SkiGrid {:val 4, :start-val 4, :row 3, :column 0, :hops 1, :propagated false}),
                       (map->SkiGrid {:val 6, :start-val 6, :row 2, :column 0, :hops 1, :propagated false}),
                       (map->SkiGrid {:val 5, :start-val 5, :row 1, :column 1, :hops 1, :propagated false}),
                       (map->SkiGrid {:val 4, :start-val 4, :row 0, :column 0, :hops 1, :propagated false}),
                       (map->SkiGrid {:val 1, :start-val 1, :row 3, :column 2, :hops 1, :propagated false}),
                       (map->SkiGrid {:val 2, :start-val 2, :row 1, :column 0, :hops 1, :propagated false}),
                       (map->SkiGrid {:val 3, :start-val 3, :row 0, :column 3, :hops 1, :propagated false}),
                       (map->SkiGrid {:val 3, :start-val 3, :row 2, :column 1, :hops 1, :propagated false}),
                       (map->SkiGrid {:val 2, :start-val 2, :row 2, :column 2, :hops 1, :propagated false}),
                       (map->SkiGrid {:val 4, :start-val 4, :row 3, :column 1, :hops 1, :propagated false}),
                       (map->SkiGrid {:val 3, :start-val 3, :row 1, :column 3, :hops 1, :propagated false})]
          queue-list# (sort node-comparator queue-list)
          fb          (FibonacciHeap.)


          entries     (reduce #(assoc %1 (get-ski-cordinates %2)
                                         (. fb (enqueue %2 (get-fibonacci-key %2)))) {} queue-list)
          _           (. fb (decreaseKey (entries (->SkiGridCordinate 0 2))
                                         (get-fibonacci-key {:val 7, :start-val 9, :row 0, :column 2, :hops 2, :propagated false})))


          _           (. fb (decreaseKey (entries (->SkiGridCordinate 2 2))
                                         (get-fibonacci-key {:val 2, :start-val 9, :row 2, :column 2, :hops 2, :propagated false})))


          _           (. fb (decreaseKey (entries (->SkiGridCordinate 1 3))
                                         (get-fibonacci-key {:val 3, :start-val 9, :row 1, :column 3, :hops 2, :propagated false})))


          _           (. fb (decreaseKey (entries (->SkiGridCordinate 1 1))
                                         (get-fibonacci-key {:val 5, :start-val 9, :row 1, :column 1, :hops 2, :propagated false})))

          _           (println (.. fb (dequeueMin) (getValue)))
          _           (println (.. fb (dequeueMin) (getValue)))
          _           (println (.. fb (dequeueMin) (getValue)))

          ]

      (is (= 1 1))
      )))



