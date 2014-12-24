(ns plantlife.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [plantlife.views :as v]
            [plantlife.branches :as b]))

(defonce app-state (atom {:text "Hello Chestnut!"
                          :branches [(b/make-fork 0 0 80 45)]}))

(defn main []
  (om/root
    (fn [app owner]
      (reify
        om/IRender
        (render [_]
          (dom/div nil
           (dom/h1 nil (:text app))
           (om/build v/svg-view (:branches app) nil)))))
    app-state
    {:target (. js/document (getElementById "app"))}))


