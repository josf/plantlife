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
  [{:keys [origin-x origin-y dest-x dest-y dest-x-cp dest-y-cp origin-x-cp origin-y-cp depth]}]
  [:path {:d (apply str
               (interpose " "
                 ["M" origin-x origin-y
                  "C" origin-x-cp origin-y-cp "," dest-x-cp dest-y-cp "," dest-x dest-y]))
          :stroke "green" :stroke-width (- 24 (* 4 depth))
          :stroke-linecap "round"
          :fill "transparent"}])


(defn derive-control-points [length angle dest-x dest-y]
  (coords-at-r-angle
    dest-x
    dest-y
    (Math.floor (* .3 length))
    ((if (= 0 (rand-int 2)) + -) angle 120)))

(defn root-branch [origin-x origin-y length sun-angle]
  (let [[dest-x dest-y] (coords-at-r-angle origin-x origin-y length sun-angle)
        [dest-x-cp dest-y-cp] (derive-control-points length sun-angle dest-x dest-y)]
    {:origin-x origin-x
     :origin-y origin-y
     :dest-x dest-x
     :dest-y dest-y
     :dest-x-cp dest-x-cp
     :dest-y-cp dest-y-cp
     :origin-x-cp (+ origin-x 10)
     :origin-y-cp origin-y
     :angle sun-angle
     :base-length length
     :length length
     :depth 0}))


(defn derive-new-angle [base negative depth]
  (Math.floor
    (let [depth-calc (if (= 0 depth) 1 depth)]
      ((if negative - +) base (/ 20 depth-calc)))))

(defn derive-new-length [base-length depth]
  (Math.floor
    (* base-length (+ .1 (rand 1.8)) (+ 1 (* 2 (/ depth 10))))))

(defn mirror-vector
  [x1 y1 x2 y2]
  [(+ (- x2 x1) x2)
   (+ (- y2 y1) y2)])

(defn derive-next [angle-coef loc]
  (let [nd (zip/node loc)
        depth (count (zip/path loc))
        new-length (derive-new-length (:base-length nd) depth)
        new-angle (derive-new-angle (:angle nd) (pos? angle-coef) depth)
        [dest-x dest-y] (coords-at-r-angle
                          (:dest-x nd) (:dest-y nd) new-length new-angle)
        [dest-x-cp dest-y-cp] (derive-control-points new-length new-angle dest-x dest-y)
        [origin-x-cp origin-y-cp] (mirror-vector
                                    (:dest-x-cp nd) (:dest-y-cp nd) (:dest-x nd) (:dest-y nd))
        new-node {:origin-x (:dest-x nd)
                  :origin-y (:dest-y nd)
                  :origin-x-cp origin-x-cp
                  :origin-y-cp origin-y-cp
                  :dest-x dest-x
                  :dest-y dest-y
                  :dest-x-cp dest-x-cp
                  :dest-y-cp dest-y-cp
                  :angle new-angle
                  :length new-length
                  :base-length (:base-length nd)
                  :depth depth}]
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
    derive-north
    derive-south))
