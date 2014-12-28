(ns plantlife.views
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [sablono.core :as html :refer-macros [html]]
            [plantlife.branches :as b]
            [plantlife.zip :as plz]
            [clojure.zip :as zip]))

(defn all-branches [zip-tree]
  (map zip/node (take-while (complement zip/end?) (iterate zip/next zip-tree))))

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
            (map b/branch-section (all-branches (plz/plant-zip branches)))]))))))
