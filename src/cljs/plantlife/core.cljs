(ns plantlife.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [plantlife.views :as v]
            [plantlife.zip :as plz]
            [clojure.zip :as zip]
            [plantlife.branches :as b]
            [plantlife.palettes :as palettes]))

(defonce app-state
  (atom {:branches []}))

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


