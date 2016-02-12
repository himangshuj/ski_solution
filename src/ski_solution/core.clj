(ns ski-solution.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string])
  (:import java.util.PriorityQueue)
  (:gen-class))


(defrecord SkiGrid [val start-val row column hops propagated]
  Object
  (toString [skigrid] (pr-str skigrid)))

(defn- get-ski-cordinates [node]
  (select-keys node [:row :column]))



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

(defn- propagate-edge [node-src node-sink]
  (if  (and
         (and node-src node-sink )
         (> (:val node-src) (:val node-sink))
         (or
           (> (:hops node-src) (:hops node-sink))
           (and (= (:hops node-src) (:hops node-sink)) (> (:start-val node-src) (:start-val node-sink)))
           )) (assoc node-sink :start-val (:start-val node-src) :hops (+ (:hops node-src) 1 ) :propagated false )
              node-sink))




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
        graph# (reduce #(assoc %1  (get-ski-cordinates %2 ) %2 ) graph neighbor-nodes#)
        node# (merge node {:propagated true})
        graph# (assoc graph# (get-ski-cordinates node# ) (map->SkiGrid node#))]
    {:graph graph#
     :nodes-modified neighbor-nodes#
     :nodes-modified-org (remove nil? [north east south west])}  ))

(def node-comparator
  (fn [node1 node2]
    (let [heuristic1 (+ (.hops node1) (.val node1))
          heuristic2 (+ (.hops node2) (.val node2))]
      (or
        (> heuristic1 heuristic2)
        (and (= heuristic1 heuristic2)
             (> (- (.start-val node1) (.val node1)) (- (.start-val node2) (.val node2))))))))



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
        _ (. priority-queue (addAll (vals input-data#) ))
        result-pq (PriorityQueue. (count input-data#) node-comparator )
        _ (println "made data into pq")
        ]  ;;side effect programming to get the top guy in priority queue
    (loop [edge-key (get-ski-cordinates (. priority-queue (poll)) )
           graph input-data#
           iteration 1] ;;initial-value
      (let [ ;;removes one element from the priority queue
            edge-to-propagate (graph edge-key)
            {new-graph :graph
             nodes-modified :nodes-modified
             nodes-modified-org :nodes-modified-org} (propagate-node edge-to-propagate graph)
            _ (println "\n------------------------------\n")
            _ (println "iteration " iteration)
            _ (println edge-to-propagate)
            _ (println "\n------------------------------ Before \n" priority-queue )
            _ (println "\n nodes modified:" nodes-modified)
            _ (println "nodes modified" (count priority-queue))
            _ (. priority-queue (removeAll nodes-modified-org))

            _ (println "\n------------------------------ During \n" priority-queue )

            _ (doseq [node nodes-modified]
                (. priority-queue (add node))
                (println "\n node :" node)
                (println "\n------------------------------ During loop \n" priority-queue ))
            _ (if (empty? nodes-modified)
                (doto result-pq
                  (.remove edge-to-propagate)
                  (.add (new-graph edge-key))))
            _ (println "\n------------------------------ After \n" priority-queue )
            _ (println "nodes modified" (count priority-queue))

            ;_ (println "pq size " (count priority-queue))
            _ (println "result pq top " (. result-pq (peek)))
            _ (println "\n------------------------------\n")]
        (if-let [new-edge (if (< iteration 1)
                            (. priority-queue (poll))) ]
          (recur
            (get-ski-cordinates new-edge)
            new-graph
            (+ 1 iteration))
          (do
            (println
              (. result-pq (poll) ))))))))
