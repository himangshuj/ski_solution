(ns ski-solution.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string])
  (:import java.util.PriorityQueue))

(defn SkiGrid->map [skigrid-obj]
  {:val (.val skigrid-obj)
   :start-val (.start-val skigrid-obj)
   :row (.row skigrid-obj)
   :column (.column skigrid-obj)
   :hops (.hops skigrid-obj)
   :propagated (.propagated skigrid-obj)})

(deftype SkiGrid [val start-val row column hops propagated]
  Object
  (equals [a b] (and (= (.row a) (.row b) )
                     (= (.column a) (.column b))))
  (hashCode [a] (* (.column a) (.row a)))
  (toString [a]  (str (SkiGrid->map a)) ))

(defn- get-ski-cordinates [ski-grid]
  {:row (.row ski-grid) :column (.column ski-grid)})


(defn map->SkiGrid [map-obj]
  (SkiGrid. (:val map-obj)
            (:start-val map-obj)
            (:row map-obj)
            (:column map-obj)
            (:hops map-obj)
            (:propagated map-obj)))





(defn- read-file [file-name]
  (with-open [rdr (io/reader (io/resource file-name) )]

    (let [lines (line-seq rdr)
          first-line (first lines)
          [length breadth] (map #(Integer/parseInt %) (string/split first-line #"\s")) ]

      {:dimensions {:length length :breadth breadth}
       :data  (doall (map
                       (fn [line row]
                         (map
                           #(map->SkiGrid {:val  (Integer/parseInt %) :row row :column %2 :hops 1 :start-val (Integer/parseInt %)
                                       :propagated false} )
                           (string/split line #"\s")
                           (iterate inc 0)))
                       (rest lines) (iterate inc 0)))})))

(defn- propagate-edge [node-src-in node-sink-in]
(if (and node-src-in node-sink-in)
  (let [node-src (SkiGrid->map node-src-in )
        node-sink (SkiGrid->map node-sink-in )]
    (if (and
          (and node-src node-sink )
          (> (:val node-src) (:val node-sink))
          (or
            (> (:hops node-src) (:hops node-sink))
            (and (= (:hops node-src) (:hops node-sink)) (> (:start-val node-src) (:start-val node-sink)))))
      (map->SkiGrid  (merge node-sink {:start-val (node-src :start-val) :hops (+ (:hops node-src) 1 ) :propagated false }))))))



(defn- propagate-node [node graph ]
  (let [ {row :row column :column} (get-ski-cordinates node)
        north (graph {:row (- row 1) :column column})
        north# (propagate-edge node north)
        south (graph {:row (+ row 1) :column column })
        south# (propagate-edge node south)
        east (graph {:row row :column (+ column 1) })
        east# (propagate-edge node east)
        west (graph {:row row :column (- column 1) })
        west# (propagate-edge node west)
        neighbor-nodes [north#  south# east# west#]
        neighbor-nodes# (remove nil? neighbor-nodes)
        nodes-come-to-play (filter #(and (-> % second  :propagated not))
                                   [ [north north#] [east east#] [west west#] [south south#]]) ;;these were earlier marked as calculated need to recalculate
        nodes-come-to-play# (map second nodes-come-to-play)
        nodes-come-to-play# (remove nil? nodes-come-to-play#)
        graph# (reduce #(merge %1  {(get-ski-cordinates %2 ) %2} ) graph neighbor-nodes#)
        node# (SkiGrid->map node)
        node# (merge node# {:propagated true})
        graph# (assoc graph# (select-keys node# [:row :Column]) (map->SkiGrid node#))]
    {:graph graph# :nodes-modified nodes-come-to-play# :nodes-modified-org neighbor-nodes# }  ))

(def node-comparator
  (fn [node1 node2]
    (or
        (> (.hops node1) (.hops node2))
        (and (= (.hops node1) (.hops node2))
             (> (- (.start-val node1) (.val node1)) (- (.start-val node2) (.val node2))))
        (and (= (.hops node1) (.hops node2))
             (= (- (.start-val node1) (.val node1)) (- (.start-val node2) (.val node2)))
             (> (.val node1) (.val node2))))))



;; we first initialize the graph with each node having the elevation stored as val
;; hops as 1 and starting elevation as start val which is the same as elevation of the node
;; now we implement modified djikstra of shortest path path1 is shorter than path2
;; if the number of hopes to reach destination node is more or drop is more than through path2
;; and final tiebreaker the elevation at path1 is more than elevation at path2

(defn -main []
  (let [input-data (read-file "input1.txt")
        input-data# (-> input-data :data flatten)
        input-data# (reduce #(merge %1 {{:row (.row %2) :column (.column %2)} %2 } ) {} input-data#)
        _ (println "made data into map")
        priority-queue  (PriorityQueue. (count input-data#) node-comparator )
        _ (. priority-queue (addAll (vals input-data#) ))]  ;;side effect programming to get the top guy in priority queue
    (loop [edge-key (get-ski-cordinates (. priority-queue (poll)) )
           graph input-data#
           iteration 1] ;;initial-value
      (let [ ;;removes one element from the priority queue
            edge-to-propagate (graph edge-key)
            {new-graph :graph
             nodes-modified :nodes-modified
             nodes-modified-org :nodes-modified-org}  (propagate-node edge-to-propagate graph)
            _ (println "\n------------------------------\n")
            _ (println "iteration " iteration)
            _ (println edge-to-propagate)
            _ (println "\n------------------------------\n")
            _ (doto priority-queue
                (.removeAll nodes-modified-org)
                (.addAll nodes-modified))
            _ (println "nodes removed " nodes-modified-org)
            _ (println "pq " priority-queue)
            _ (println "\n------------------------------\n")]
        (if-let [new-edge (. priority-queue (poll))]
          (recur
            (get-ski-cordinates new-edge)
            new-graph
            (+ 1 iteration))
          (do
            (. priority-queue (addAll (vals new-graph) ))
            (println
              (. priority-queue (poll) ))))))))
