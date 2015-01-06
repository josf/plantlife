(ns plantlife.branches
  (:require
   [clojure.zip :as zip]
   [plantlife.zip :as plz]
   [plantlife.palettes :as palettes]))


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


(defn choose-color [& current]
  (let [cur (first current)]
    (case (rand-int 3)
      0 (:green palettes/palettes)
      1 (:yellow palettes/palettes)
      2 (:blue palettes/palettes)
      3 (:brown palettes/palettes))))

(defn derive-control-points [length angle dest-x dest-y]
  (coords-at-r-angle
    dest-x
    dest-y
    (Math.floor (* .3 length))
    ((if (= 0 (rand-int 2)) + -) angle 120)))

(defn seg-length [origin-x origin-y dest-x dest-y]
  (let [x-diff (- (max origin-x dest-x) (min origin-x dest-x))
        y-diff (- (max origin-y dest-y) (min origin-y dest-y))]
    (Math.floor
      (Math.sqrt
        (+ (Math.pow x-diff 2) (Math.pow y-diff 2))))))

(defn root-branch [origin-x origin-y length sun-angle palette]
  (let [[dest-x dest-y] (coords-at-r-angle origin-x origin-y length sun-angle)
        [dest-x-cp dest-y-cp] (derive-control-points length sun-angle dest-x dest-y)]
    {:origin-x origin-x
     :origin-y origin-y
     :dest-x dest-x
     :dest-y dest-y
     :current-x origin-x
     :current-y origin-y
     :dest-x-cp dest-x-cp
     :dest-y-cp dest-y-cp
     :origin-x-cp (+ origin-x 10)
     :origin-y-cp origin-y
     :angle sun-angle
     :base-length length
     :length length
     :depth 0
     :children []
     :palette palette
     :color (nth palette (rand-int (count palette)))}))

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
                  :current-x (:dest-x nd)
                  :current-y (:dest-y nd)
                  :dest-x dest-x
                  :dest-y dest-y
                  :dest-x-cp dest-x-cp
                  :dest-y-cp dest-y-cp
                  :angle new-angle
                  :length new-length
                  :base-length (:base-length nd)
                  :depth depth
                  :children []
                  :palette (:palette nd)
                  :color (nth (:palette nd) (rand-int (count (:palette nd))))}]
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


(defn min-avail-leaf-depth
  "Find the minium tree depth of branch nodes that could have at
  least one more child branch."
  [z]
  (apply min
    (map #(count (zip/path %))
      (filter (fn [l] (and
                        (zip/branch? l)
                        (> 2 (count (zip/children l)))))
        (take-while (complement zip/end?) (iterate zip/next z))))))


(defn full-length-branch?
  "current-x and current-y equal dest-x and dest-y"
  [loc]
  (let [nd (zip/node loc)]
   (and
     (= (:current-x nd) (:dest-x nd))
     (= (:current-y nd) (:dest-y nd)))))

(defn next-available-branch [loc min-depth]
  (first
    (filter
      (fn [l]
        (and
          (zip/branch? l)
          (= min-depth (count (zip/path l)))
          (full-length-branch? l)
          (> 2 (count (zip/children l)))))
      (take-while (complement zip/end?) (iterate zip/next loc)))))

(defn add-next-branch [branches-cursor]
  (let [max-depth 5
        bzip (plz/plant-zip branches-cursor)
        min-depth (min-avail-leaf-depth bzip)]

    (if (>= min-depth max-depth)
      branches-cursor
      (let [target-loc (next-available-branch bzip min-depth)
            new-branches (when target-loc
                           (zip/edit
                             target-loc
                             (fn [n]
                               (cond
                                 (zero? (count (:children n)))
                                (assoc n :children [(derive-north target-loc)])

                                (= 1 (count (:children n)))
                                (assoc n :children
                                  (conj (:children n)
                                    (derive-south target-loc)))))))]
      
        (if new-branches
          (zip/root new-branches)
          branches-cursor)))))

(defn branches-full? [loc]
  (let [filtered  (filter
                    (fn [l]
                      (and
                        (zip/branch? l)
                        (= (count (zip/path l)) 5)))
                    (take-while (complement zip/end?) (iterate zip/next loc)))]
    (and
      (not (empty? filtered))
      (every? full-length-branch? filtered))))

(defn increment-branch-lengths
  [origin-x origin-y dest-x dest-y current-x current-y]
  (let [x-diff (- dest-x origin-x)
        y-diff (- dest-y origin-y)
        x-incr (if (> (Math.abs (/ x-diff 80)) 1)
                 (/ x-diff 80)
                 (if (pos? x-diff) 1 -1))
        y-incr (if (> (Math.abs (/ y-diff 80)) 1)
                 (/ y-diff 80)
                 (if (pos? y-diff) 1 -1))]
    [(if (> (Math.abs x-incr) (Math.abs (- dest-x current-x)))
       dest-x
       (Math.floor (+ current-x x-incr)))

     (if (> (Math.abs y-incr) (Math.abs (- dest-y current-y)))
       dest-y
       (Math.floor (+ current-y y-incr)))]))


(defn all-branches-full-length? [branches-zip]
  (every?
    #(full-length-branch? %)
    (filter zip/branch?
      (take-while
        (complement zip/end?)
        (iterate zip/next branches-zip)))))

(defn step-incomplete-branches [branches-cursor]
  (zip/root 
   (loop [loc (plz/plant-zip branches-cursor)]
     (cond
       (zip/end? loc)
       loc
      
       (not (zip/branch? loc))
       (recur (zip/next loc))

       (full-length-branch? loc)
       (recur (zip/next loc))

       true
       (recur
         (zip/next
           (zip/edit
             loc
             (fn [n]
               (let [[new-x new-y] (increment-branch-lengths
                                     (:origin-x n)
                                     (:origin-y n)
                                     (:dest-x n)
                                     (:dest-y n)
                                     (:current-x n)
                                     (:current-y n))]
                 (assoc n
                   :current-x new-x
                   :current-y new-y))))))))))
