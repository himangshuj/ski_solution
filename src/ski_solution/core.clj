(ns ski-solution.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string])
  (:import [ski_solution.util FibonacciHeap])
  (:refer-clojure :exclude [min])
  (:gen-class))


(defrecord SkiGrid [val start-val row column hops propagated prev]
  Object
  (toString [skigrid] (str (pr-str skigrid) "  heuristic " (+ (:val skigrid) (:hops skigrid)))))
(defrecord SkiGridCordinate [row column])

(defn get-ski-cordinates [node]
  (map->SkiGridCordinate (select-keys node [:row :column])))

(def depth-multiplier (/ 1 2000.0))                         ;; this will ensure hops always overlord depth

(defn get-fibonacci-key [node]
  (* -1 (+ (:val node) (:hops node)
           (* depth-multiplier (- (:start-val node) (:val node))))))


(defn get-result-key [node]
  (* -1 (+ (:hops node)
           (* depth-multiplier (- (:start-val node) (:val node))))))

(defrecord SkiGridCordinate [row column])

(defn- read-file [file-name]
  (with-open [rdr (io/reader (io/resource file-name))]

    (let [lines      (line-seq rdr)
          first-line (first lines)
          [length breadth] (map #(Integer/parseInt %) (string/split first-line #"\s"))]

      {:dimensions {:length length :breadth breadth}
       :data       (doall (map
                            (fn [line row]
                              (map
                                #(map->SkiGrid {:val        (Integer/parseInt %) :row row :column %2 :hops 1 :start-val (Integer/parseInt %)
                                                :propagated false})
                                (string/split line #"\s")
                                (iterate inc 0)))
                            (rest lines) (iterate inc 0)))})))

(defn- propagate-edge [node-src node-sink]
  (if (and
        (and node-src node-sink)
        (> (:val node-src) (:val node-sink))
        (or
          (> (:hops node-src) (:hops node-sink))
          (and (= (:hops node-src) (:hops node-sink)) (> (:start-val node-src) (:start-val node-sink)))
          )) (assoc node-sink :start-val (:start-val node-src) :hops (+ (:hops node-src) 1) :propagated false :prev node-src)))



(defn- get-node-val [graph ski-cordinates]
  (if-let [node (graph ski-cordinates)]
    (. node (getValue))))

(defn- propagate-node [node graph]
  (let [{row :row column :column} (get-ski-cordinates node)
        north           (get-node-val graph (->SkiGridCordinate (- row 1) column))
        north#          (propagate-edge node north)
        south           (get-node-val graph (->SkiGridCordinate (+ row 1) column))
        south#          (propagate-edge node south)
        east            (get-node-val graph (->SkiGridCordinate row (- column 1)))
        east#           (propagate-edge node east)
        west            (get-node-val graph (->SkiGridCordinate row (+ column 1)))
        west#           (propagate-edge node west)
        neighbor-nodes  [north# south# east# west#]
        neighbor-nodes# (remove nil? neighbor-nodes)
        _               (doseq [node neighbor-nodes#]
                          (let [cordinates (get-ski-cordinates node)
                                original   (graph cordinates)]
                            (. original (setValue node))))
        node#           (merge node {:propagated true})]
    {:graph          graph
     :nodes-modified neighbor-nodes#}))

(def node-comparator
  (fn [node1 node2]
    (let [heuristic1 (+ (.hops node1) (.val node1))
          depth1     (* depth-multiplier (- (:start-val node1) (:val node1)))
          heuristic2 (+ (.hops node2) (.val node2))
          depth2     (* depth-multiplier (- (:start-val node2) (:val node2)))]
      (or
        (> (+ heuristic1 depth1) (+ heuristic2 depth2))))))




;; we first initialize the graph with each node having the elevation stored as val
;; hops as 1 and starting elevation as start val which is the same as elevation of the node
;; now we implement modified A* of shortest path path1 is shorter than path2
;; if the number of hopes to reach destination node is more or drop is more than through path2
;; and final tiebreaker the elevation at path1 is more than elevation at path2
;; for heuristics, the number of hops at each node is set as the elevation

(defn -main []
  (let [input-data  (read-file "map.txt")
        input-data# (-> input-data :data flatten)
        fb-heap     (FibonacciHeap.)
        input-data# (reduce #(assoc %1 (map->SkiGridCordinate {:row (.row %2) :column (.column %2)})
                                       (.. fb-heap (enqueue %2 (get-fibonacci-key %2)))) {} input-data#) ;;find it easier to replace the value of node in place
        _           (println "made data into map")
        result-heap (FibonacciHeap.)]                       ;;side effect programming to get the top guy in priority queue
    (println "made data into heap ")
    (loop [edge-to-propagate (. fb-heap (dequeueMin))
           graph             input-data#
           iteration         1]                             ;;initial-value
      (let [
            edge-to-propagate# (. edge-to-propagate (getValue))
            {new-graph      :graph
             nodes-modified :nodes-modified} (propagate-node edge-to-propagate# graph)

            -                  (println "\n----------------\n")
            _                  (println "iteration " iteration)
            _                  (println edge-to-propagate#)
            _                  (println "nodes modfied" nodes-modified)
            -                  (println "\n----------------\n")

            _                  (doseq [node nodes-modified]
                                 (. fb-heap (decreaseKey (graph (get-ski-cordinates node))
                                                         (get-fibonacci-key node)))) ;; only lower vertices can be moved to so this will be always lesser than prev as our keys are all not negative
            _                  (if (empty? nodes-modified)
                                 (do
                                   (doto result-heap
                                     (.enqueue (. edge-to-propagate (getValue)) (get-result-key edge-to-propagate#))))) ;; the actual-value will change to something lesser even with duplicates i dont care

            ;_ (println "pq size " (count priority-queue))
            current-leader     (if-let [cl (. result-heap (min))]
                                 (. cl (getValue)))
            new-edge           (. fb-heap (dequeueMin))
            _                   (println "result heap top " current-leader " comparision " (if current-leader (get-result-key current-leader) ) )
            _                   (println "new edge is" (. new-edge (getValue)) " comparision" (get-fibonacci-key (. new-edge (getValue)) ))

            ]
        (if (and new-edge
                 (or
                   (nil? current-leader)                    ;; there is no current leader
                   (< (get-fibonacci-key (. new-edge (getValue)))
                      (get-result-key current-leader))      ;; the heuristic value of new edge is higher than or equal current leader
                   ))
          (recur
            new-edge
            new-graph
            (+ 1 iteration))
          (do
            (println
              (.. result-heap (dequeueMin) (getValue)))))))))
