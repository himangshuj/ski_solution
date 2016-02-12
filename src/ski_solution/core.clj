(ns ski-solution.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn- read-file [file-name]
  (with-open [rdr (io/reader (io/resource file-name) )]

    (let [lines (line-seq rdr)
          first-line (first lines)
          [length breadth] (map #(Integer/parseInt %) (string/split first-line #"\s")) ]

      {:dimensions {:length length :breadth breadth}
       :data  (doall (map
                       (fn [line row]
                         (map
                           #(hash-map :val  (Integer/parseInt %) :row row :column %2 :hops 1 :start-val (Integer/parseInt %)
                                      :propagated false)
                           (string/split line #"\s")
                           (iterate inc 0)))
                       (rest lines) (iterate inc 0)))})))

(defn- propagate-edge [node-src node-sink]
  (if (and
        (and node-src node-sink )
        (> (:val node-src) (:val node-sink))
        (or
          (> (:hops node-src) (:hops node-sink))
          (and (= (:hops node-src) (:hops node-sink)) (> (:start-val node-src) (:start-val node-sink)))))
    (merge node-sink {:start-val (node-src :start-val) :hops (+ (:hops node-src) 1 ) :propagated false })))

(defn- propagate-node [node graph ]
  (let [ {row :row column :column} (select-keys node [:row :column])
        north (graph {:row (- row 1) :column column})
        north# (propagate-edge node north)
        south (graph {:row (+ row 1) :column column })
        south# (propagate-edge node south)
        east (graph {:row row :column (+ column 1) })
        east# (propagate-edge node east)
        west (graph {:row row :column (- column 1) })
        west# (propagate-edge node west)
        new-nodes [north#  south# east# west#]
        new-nodes# (remove nil? new-nodes)
        graph# (reduce #(merge %1  {(select-keys %2 [:row :column]) %2} ) graph new-nodes#)
        node# (merge node {:propagated true})
        graph# (merge graph# {(select-keys node [:row :column]) node#})]
    graph#))

(def node-comparator
  (fn [node1 node2]
    (or
        (> (:hops node1) (:hops node2))
        (and (= (:hops node1) (:hops node2))
             (> (- (:start-val node1) (:val node1)) (- (:start-val node2) (:val node2))))
        (and (= (:hops node1) (:hops node2))
             (= (- (:start-val node1) (:val node1)) (- (:start-val node2) (:val node2)))
             (> (:val node1) (:val node2)))
        )
    ))

;; we first initialize the graph with each node having the elevation stored as val
;; hops as 1 and starting elevation as start val which is the same as elevation of the node
;; now we implement modified djikstra of shortest path path1 is shorter than path2
;; if the number of hopes to reach destination node is more or drop is more than through path2
;; and final tiebreaker the elevation at path1 is more than elevation at path2

(defn -main []
  (let [input-data (read-file "input1.txt")
        input-data# (-> input-data :data flatten)
        input-data# (reduce #(merge %1 {{:row (:row %2) :column (:column %2)} %2 } ) {} input-data#)
        sorted-input-data (sort node-comparator (vals input-data#))]

    (loop [edge-key (select-keys (first sorted-input-data) [:row :column])
           graph input-data#
           iteration 1]
      (let [
            edge-to-propagate (graph edge-key)
            new-graph (propagate-node edge-to-propagate graph)
            unprocessed-graph (remove #(-> %  :propagated) (vals new-graph))
            sorted-unprocessed (sort node-comparator unprocessed-graph )
            new-edge-to-propagate (first sorted-unprocessed)]
        (if new-edge-to-propagate
          (recur (select-keys new-edge-to-propagate [:row :column])
                 new-graph
                 (+ 1 iteration))
          (do
            (println
              (first (sort node-comparator (vals new-graph)) ))))))))
