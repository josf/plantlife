(ns plantlife.views
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [sablono.core :as html :refer-macros [html]]))


(defn branch-view [branch owner]
  (reify
    om/IDisplayName
    (display-name [_] "Branch")

    om/IRenderState
    (render-state [_ state]
      (html
        (branch-section
          [(:origin-x branch) (:origin-y branch)]
          [(:dest-x branch) (:dest-y branch)])))))


(defn svg-view [data owner]
  (reify
    om/IDisplayName
    (display-name [_] "SvgRoot")

    om/IRenderState
    (render-state [_ state]
      (html
        [:svg {:version "1.1" :width 1200 :height 800}
         (om/build branch-view {:origin-x 20 :origin-y 20 :dest-x 100 :dest-y 300})
         ]))))
