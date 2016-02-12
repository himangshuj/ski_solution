(ns ski-solution.core-test
  (:require [clojure.test :refer :all]
            [ski-solution.core :refer :all]))



(deftest comparision-test
  (testing "comparision tests"
    (let [vals [{:start-val 4, :val 4, :hops 0, :column 0, :propagated false, :row 0}
                {:start-val 9, :val 9, :hops 1, :column 2, :propagated false, :row 1}
                {:start-val 3, :val 3, :hops 2, :column 1, :propagated false, :row 2}
                {:start-val 5, :val 5, :hops 0, :column 3, :propagated false, :row 2}
                {:start-val 5, :val 5, :hops 0, :column 1, :propagated false, :row 1}
                {:start-val 6, :val 6, :hops 0, :column 0, :propagated false, :row 2}
                {:start-val 4, :val 4, :hops 0, :column 0, :propagated false, :row 3}
                {:start-val 8, :val 6, :hops 0, :column 3, :propagated false, :row 3} ]
          vals-sorted (sort node-comparator vals) ]
      (are [x y] (= x y)
                 (first vals-sorted)
                 {:start-val 3, :val 3, :hops 2, :column 1, :propagated false, :row 2}
                 (second vals-sorted)
                 {:start-val 9, :val 9, :hops 1, :column 2, :propagated false, :row 1}
                 (nth vals-sorted 2)
                 {:start-val 8, :val 6, :hops 0, :column 3, :propagated false, :row 3}
                 (nth vals-sorted 3)
                 {:start-val 6, :val 6, :hops 0, :column 0, :propagated false, :row 2}))))
