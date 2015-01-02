(ns plantlife.zip
  (:require
   [clojure.zip :as zip]))


(defn plant-zip [tree]
  (zip/zipper
    ;;  branch?: all nodes can have children, but a nil value as child can't
    (fn [n] (and n (not (vector? n))))
    (fn [t]
      (let [cs (filter identity (:children t))]
        (when (pos? (count cs))
          cs)))
    (fn [nod chils]
      (assoc nod :children (vec (take 2 chils))))
    tree))


