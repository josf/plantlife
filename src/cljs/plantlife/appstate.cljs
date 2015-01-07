(ns plantlife.appstate
  (:require
    [om.core :as om :include-macros true]
    [om.dom :as dom :include-macros true]))


(defonce app-state
  (atom {:branches []
         :dimensions {:height nil :width nil}}))

(defn dimensions []
  (om/ref-cursor (:dimensions (om/root-cursor app-state))))

