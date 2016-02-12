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
                           #(hash-map :val  (Integer/parseInt %) :row row :column %2 :hops 0 :start-val (Integer/parseInt %)
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
    (merge node-sink {:start-val (node-src :start-val) :hops (+ (:hops node-src) 1 ) })))

(defn- propagate-node [node graph ]
  (let [ {row :row column :column} (select-keys node [:row :column])
        north (graph {:row (- row 1) :column column})
        north# (propagate-edge node north)
        north-west (graph {:row (- row 1) :column (- column 1) })
        north-west# (propagate-edge node north-west)
        north-east (graph {:row (- row 1) :column (+ column 1) })
        north-east# (propagate-edge node north-east)
        south-east (graph {:row (+ row 1) :column (+ column 1) })
        south-east# (propagate-edge node south-east)
        south (graph {:row (+ row 1) :column column })
        south# (propagate-edge node south)
        south-west (graph {:row (+ row 1) :column (- column 1) })
        south-west# (propagate-edge node south-west)
        new-nodes [north# north-east# north-west# south# south-west# south-east#]
        new-nodes# (remove nil? new-nodes)
        graph# (reduce #(merge %1  {(select-keys %2 [:row :column]) %2} ) graph new-nodes#)
        graph# (update-in graph# (select-keys node [:row :column]  )
                          #(merge % {:propagated true}))
        ]
    graph#
    ))
(def node-comparator
  (fn [node1 node2]
    (or (:propagated node2)
        (> (:hops node1) (:hops node2))
        (and (= (:hops node1) (:hops node2))
             (> (:val node1) (:val node2)))
        (and (= (:hops node1) (:hops node2))
             (= (:val node1) (:val node2))
             (> (:start-val node1) (:start-val node2)))
        )
    ))


(defn -main []
  (let [input-data (read-file "input1.txt")
        input-data# (-> input-data :data flatten)
        input-data# (reduce #(merge %1 {{:row (:row %2) :column (:column %2)} %2 } ) {} input-data#)
        sorted-input-data (sort node-comparator (vals input-data#))
        edge-to-propagate (input-data# (select-keys (first sorted-input-data) [:row :column]) )
        ]
    (println (vals input-data#))
    (println edge-to-propagate)
    )
  (println "aaa"))
