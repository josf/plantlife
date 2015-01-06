(ns plantlife.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [clojure.zip :as zip]
            [sablono.core :as html :refer-macros [html]]
            [goog.dom :as gdom]
            [goog.dom.ViewportSizeMonitor :as vsm]
            [plantlife.zip :as plz]
            [plantlife.branches :as b]
            [plantlife.palettes :as palettes]))

(defonce app-state
  (atom {:branches []
         :dimensions {:height nil :width nil}}))


(defn all-branches [zip-tree]
  (map zip/node (take-while (complement zip/end?) (iterate zip/next zip-tree))))


(defn branch-view [branch owner]
  (reify
    om/IDisplayName
    (display-name [_] "BranchPath")

    om/IRenderState
    (render-state [_ state]
      (let [{:keys [origin-x origin-y dest-x dest-y length angle dest-x-cp dest-y-cp
                    origin-x-cp origin-y-cp current-x current-y depth color]} branch
                    
                    current-x-cp current-x
                    current-y-cp current-y

                    actual-x-cp (if (= dest-x current-x) dest-x-cp current-x-cp)
                    actual-y-cp (if (= dest-y current-y) dest-y-cp current-y-cp)]
        (html
          [:path {:d (apply str
                       (interpose " "
                         ["M" origin-x origin-y
                          "C" origin-x-cp origin-y-cp "," actual-x-cp actual-y-cp "," current-x current-y]))
                  :stroke color :stroke-width (- 24 (* 4 depth))
                  :stroke-linecap "round"
                  :fill "transparent"}])))))


(defn start-angle [origin-x origin-y width height]
  (let [center-x (/ width 2)
        delta-x (- center-x origin-x)
        delta-y (- origin-y 0)]
    (if (= 0 delta-x)
      -90
      (- (Math.floor
           (*
             (Math.atan2 delta-y delta-x)
             (/ 180 Math.PI)))))))


(defn random-start-point [width height]
  (let [x (rand-int width)
        y height
        ang  (start-angle x y width height)]
    (println ang)
   [x y ang]))


(defn svg-view [app owner]
  (reify
    om/IDisplayName
    (display-name [_] "SvgRoot")

    om/IInitState
    (init-state [_]
      {:viewport-mon (vsm/getInstanceForWindow)})

    om/IWillMount
    (will-mount [_]
      (let [vpm (om/get-state owner :viewport-mon)
            size (.getSize vpm)
            height (.-height size)
            width (.-width size)]
        (om/set-state! owner :height height)
        (om/set-state! owner :width width)

        ;; our initial branch when mounting for the first time
        (when (empty? (:branches @app))
          (om/transact! app :branches
            (fn [_]
             (b/root-branch
               (Math.floor (/ width 2))
               height
               (Math.floor (/ (min height width) 6))
               -90
               (:green palettes/palettes)))))

        
        (js/setInterval
          (fn []
            (om/transact! app :branches b/add-next-branch))
          500)

        (js/setInterval
          (fn []
            (om/transact! app :branches b/step-incomplete-branches))
          42)

        (js/setInterval
          (fn []
            (om/transact! app :branches
              (fn [br]
                (let [bzip  (plz/plant-zip br)]
                  (if (and
                        (b/branches-full? bzip)
                        (b/all-branches-full-length? bzip))
                    (let [prev-root-color (:palette (zip/root bzip))
                          [new-x new-y new-angle] (random-start-point width height)
                          branch-length (Math.floor (/ (min height width) 6))]
                      (b/root-branch
                        new-x
                        new-y
                        branch-length
                        new-angle
                        (b/choose-color prev-root-color))) 
                    br)))))
          505)))

    om/IRenderState
    (render-state [_ state]
      (html
        (vec
         (concat
           [:svg {:version "1.1" :width (:width state) :height (:height state)}
            (om/build-all branch-view (all-branches (plz/plant-zip (:branches app))))]))))))



(defn main []
  (om/root
    (fn [app owner]
      (reify
        om/IRender
        (render [_]
          (dom/div nil
           (om/build svg-view app nil)))))
    app-state
    {:target (. js/document (getElementById "app"))}))


