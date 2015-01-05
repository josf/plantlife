(ns plantlife.zip
  (:require
   [clojure.zip :as zip]))


(defn plant-zip [tree]
  (zip/zipper
    identity
    (fn [t]
      (let [cs (filter identity (:children t))]
        (when (pos? (count cs))
          cs)))
    (fn [nod chils]
      (assoc nod :children (vec (take 2 chils))))
    tree))


