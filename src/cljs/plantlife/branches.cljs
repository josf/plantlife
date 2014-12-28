(ns plantlife.branches
  (:require
   [clojure.zip :as zip]
   [plantlife.zip :as plz]))


(defn coords-at-r-angle
  [origin-x origin-y length sun-angle]
  (let [rad-angle  (* sun-angle (/ Math.PI 180))
        x (* length (Math.cos rad-angle))
        y (* length (Math.sin rad-angle))]
    [(Math.floor (+ origin-x x))
     (Math.floor (+ origin-y y))]))


(defn branch-section
  ([{origin-x :origin-x origin-y :origin-y dest-x :dest-x dest-y :dest-y}]
   (branch-section [origin-x origin-y] [dest-x dest-y]))
  ([[origin-x origin-y] [dest-x dest-y]]
   [:path {:d (apply str (interpose " " ["M" origin-x origin-y "L" dest-x dest-y]))
           :stroke "green" :stroke-width 5 :stroke-linecap "round" :fill "transparent"}]))


(defn root-branch [origin-x origin-y length sun-angle]
  (let [[dest-x dest-y] (coords-at-r-angle origin-x origin-y length sun-angle)]
    {:origin-x origin-x
     :origin-y origin-y
     :dest-x dest-x
     :dest-y dest-y
     :angle sun-angle
     :length length}))

(defn derive-next [angle-coef loc]
  (let [nd (zip/node loc)
        new-angle (Math.floor (* angle-coef (:angle nd)))
        [dest-x dest-y] (coords-at-r-angle
                          (:dest-x nd) (:dest-y nd) (:length nd) new-angle)
        new-node {:origin-x (:dest-x nd)
                  :origin-y (:dest-y nd)
                  :dest-x dest-x
                  :dest-y dest-y
                  :angle new-angle
                  :length (:length nd)}]
    new-node))

(def derive-north (partial derive-next 1.2))
(def derive-south (partial derive-next 0.8))

(defn build-tree [loc max-depth derive-north-fn derive-south-fn]
  "derive-north-fn and derive-south-fn take locs so they can be (and
  must be!) zipper aware."
  (cond
    (zip/end? loc)
    loc

    (or
      (= max-depth (count (zip/path loc)))
      (= 2 (count (zip/children loc))))
    (recur (zip/next loc)
      max-depth
      derive-north-fn
      derive-south-fn)

    (= 1 (count (zip/children loc)))
    (recur (zip/append-child loc (derive-south-fn loc))
      max-depth
      derive-north-fn
      derive-south-fn)

    (= 0 (count (zip/children loc)))
    (recur (zip/insert-child loc (derive-north-fn loc))
      max-depth
      derive-north-fn
      derive-south-fn)))


(defn complete-tree [origin-x origin-y length sun-angle max-depth]
  (build-tree
    (plz/plant-zip (root-branch origin-x origin-y length sun-angle))
    max-depth
    derive-north-fn
    derive-south-fn))
