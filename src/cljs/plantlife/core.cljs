(ns plantlife.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [plantlife.views :as v]
            [plantlife.zip :as plz]
            [clojure.zip :as zip]
            [plantlife.branches :as b]))

(defonce app-state
  (atom {:text "Hello Chestnut!"
         :branches (zip/node
                     (b/build-tree
                       (plz/plant-zip (b/root-branch 0 0 50 45))
                       2
                       b/derive-north b/derive-south))}))

(defn main []
  (om/root
    (fn [app owner]
      (reify
        om/IRender
        (render [_]
          (dom/div nil
           (om/build v/svg-view (:branches app) nil)))))
    app-state
    {:target (. js/document (getElementById "app"))}))


