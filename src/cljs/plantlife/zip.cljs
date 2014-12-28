(ns plantlife.zip
  (:require
   [clojure.zip :as zip]))


(defn plant-zip [tree]
  (zip/zipper
    ;;  branch?: all nodes can have children, but a nil value as child can't
   (fn [n] (not (nil? n)))             
    (fn [t]
      (let [cs (filter identity [(:north-child t) (:south-child t)])]
        (when (pos? (count cs))
          cs)))
    (fn [nod chils]
      (-> nod
        (assoc :north-child (first chils))
        (assoc :south-child (second chils))))
    tree))


