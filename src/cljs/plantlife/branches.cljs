(ns plantlife.branches)


(defn coords-at-r-angle
  [origin-x origin-y length sun-slope]
  (let [rad-slope  (* sun-slope (/ Math.PI 180))
        x (* length (Math.cos rad-slope))
        y (* length (Math.sin rad-slope))]
    [(Math.floor (+ origin-x x))
     (Math.floor (+ origin-y y))]))


(defn branch-section
  [[origin-x origin-y] [dest-x dest-y]]
  [:path {:d (apply str (interpose " " ["M " origin-x origin-y "L" dest-x dest-y]))
    :stroke "green" :stroke-width 5 :stroke-linecap "round" :fill "transparent"}])

(defn make-fork [origin-x origin-y length sun-slope]
  (let [[stem-end-x stem-end-y] (coords-at-r-angle origin-x origin-y length sun-slope)
        top-fork-slope (+ 20 sun-slope)
        top-fork-end (coords-at-r-angle stem-end-x stem-end-y length top-fork-slope)
        bottom-fork-slope (- 20 sun-slope)
        bottom-fork-end (coords-at-r-angle stem-end-x stem-end-y length bottom-fork-slope)]
    {:stem (branch-section [origin-x origin-y] [stem-end-x stem-end-y])
     :top-fork (branch-section [stem-end-x stem-end-y] top-fork-end)
     :top-fork-end top-fork-end
     :top-fork-slope top-fork-slope
     :bottom-fork (branch-section [stem-end-x stem-end-y] bottom-fork-end)
     :bottom-fork-end bottom-fork-end
     :bottom-fork-slope bottom-fork-slope}))

(defn next-forks [{:keys [stem top-fork-end top-fork-slope bottom-fork-end bottom-fork-slope]} length]
  [(make-fork (first top-fork-end) (second top-fork-end) length (+ 20 top-fork-slope))
   (make-fork (first bottom-fork-end) (second bottom-fork-end) length (- 20 bottom-fork-slope))])
