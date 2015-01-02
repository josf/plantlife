(ns plantlife.views
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [sablono.core :as html :refer-macros [html]]
            [plantlife.branches :as b]
            [plantlife.zip :as plz]
            [clojure.zip :as zip]))

(defn all-branches [zip-tree]
  (map zip/node (take-while (complement zip/end?) (iterate zip/next zip-tree))))


(defn branch-view [branch owner]
  (reify
    om/IDisplayName
    (display-name [_] "BranchPath")

    om/IRenderState
    (render-state [_ state]
      (let [{:keys [origin-x origin-y dest-x dest-y dest-x-cp dest-y-cp origin-x-cp origin-y-cp depth]} branch]
        (html
          [:path {:d  (apply str
                        (interpose " "
                          ["M" origin-x origin-y
                           "C" origin-x-cp origin-y-cp "," dest-x-cp dest-y-cp "," dest-x dest-y]))
                  :stroke "green" :stroke-width (- 24 (* 4 depth))
                  :stroke-linecap "round"
                  :fill "transparent"}])))))

(defn svg-view [branches owner]
  (reify
    om/IDisplayName
    (display-name [_] "SvgRoot")

    om/IRenderState
    (render-state [_ state]
      (println branches)
      (html
        (vec
         (concat
           [:svg {:version "1.1" :width 1200 :height 800}
            (om/build-all branch-view (all-branches (plz/plant-zip branches)))]))))))
