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
  ([{origin-x :origin-x origin-y :origin-y dest-x :dest-x
     dest-y :dest-y dest-x-cp :dest-x-cp dest-y-cp :dest-y-cp}]
   (branch-section [origin-x origin-y] [dest-x dest-y] [dest-x-cp dest-y-cp]))
  ([[origin-x origin-y] [dest-x dest-y] [dest-x-cp dest-y-cp]]
   [:path {:d (apply str
                (interpose " "
                  ["M" origin-x origin-y
                   "C" (+ origin-x 20) (- origin-y 20) "," (- dest-x 20) (+ dest-y 20) "," dest-x dest-y]))
           :stroke "green" :stroke-width 8 :stroke-linecap "round" :fill "transparent"}]))


(defn derive-control-points [length angle dest-x dest-y]
  [(- dest-x 20) (- dest-y 20)])

(defn root-branch [origin-x origin-y length sun-angle]
  (let [[dest-x dest-y] (coords-at-r-angle origin-x origin-y length sun-angle)
        [dest-x-cp dest-y-cp] (derive-control-points length sun-angle dest-x dest-y)]
    {:origin-x origin-x
     :origin-y origin-y
     :dest-x dest-x
     :dest-y dest-y
     :dest-x-cp dest-x-cp
     :dest-y-cp dest-y-cp
     :angle sun-angle
     :base-length length
     :length length}))


(defn derive-new-angle [base negative depth]
  (Math.floor
    (let [depth-calc (if (= 0 depth) 1 depth)]
      ((if negative - +) base (/ 20 depth-calc)))))

(defn derive-new-length [base-length]
  (Math.floor
    (* base-length (+ .3 (rand 1.4)))))

(defn derive-next [angle-coef loc]
  (let [nd (zip/node loc)
        depth (count (zip/path loc))
        new-length (derive-new-length (:base-length nd))
        new-angle (derive-new-angle (:angle nd) (pos? angle-coef) depth)
        [dest-x dest-y] (coords-at-r-angle
                          (:dest-x nd) (:dest-y nd) new-length new-angle)
        [dest-x-cp dest-y-cp] (derive-control-points new-length new-angle dest-x dest-y)
        new-node {:origin-x (:dest-x nd)
                  :origin-y (:dest-y nd)
                  :dest-x dest-x
                  :dest-y dest-y
                  :dest-x-cp dest-x-cp
                  :dest-y-cp dest-y-cp
                  :angle new-angle
                  :length new-length
                  :base-length (:base-length nd)}]
    new-node))

(def derive-north (partial derive-next 15))
(def derive-south (partial derive-next -15))

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
