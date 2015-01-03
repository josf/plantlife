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
      (let [{:keys [origin-x origin-y dest-x dest-y length angle dest-x-cp dest-y-cp
                    origin-x-cp origin-y-cp current-x current-y depth complete]} branch
                    
                    current-x-cp current-x
                    current-y-cp current-y

                    actual-x-cp (if (= dest-x current-x) dest-x-cp current-x-cp)
                    actual-y-cp (if (= dest-y current-y) dest-y-cp current-y-cp)]
        (html
          [:path {:d (apply str
                       (interpose " "
                         ["M" origin-x origin-y
                          "C" origin-x-cp origin-y-cp "," actual-x-cp actual-y-cp "," current-x current-y]))
                  :stroke "green" :stroke-width (- 24 (* 4 depth))
                  :stroke-linecap "round"
                  :fill "transparent"}])))))

(defn svg-view [branches owner]
  (reify
    om/IDisplayName
    (display-name [_] "SvgRoot")

    om/IWillMount
    (will-mount [_]
      (js/setInterval
        (fn []
         (om/transact! branches b/add-next-branch))
        500)
      (js/setInterval
        (fn []
          (om/transact! branches b/step-incomplete-branches))
        16))

    
    om/IRenderState
    (render-state [_ state]
      (html
        (vec
         (concat
           [:svg {:version "1.1" :width 1200 :height 800}
            (om/build-all branch-view (all-branches (plz/plant-zip branches)))]))))))
