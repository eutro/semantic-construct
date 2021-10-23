(ns semantic-construct.screen.main-menu
  (:require [semantic-construct.screen.screen :as screen]
            [semantic-construct.screen.ui :as ui]
            [semantic-construct.mouse :as mouse]
            [semantic-construct.state :as state]
            [semantic-construct.theme :as theme]
            [semantic-construct.render :as r]))

(def default-state
  {:screen {:type :main-menu
            :scene
            (r/->Scene
             [(r/obj-with-bindings r/fill :fillStyle (:background theme/theme))
              (ui/menu-button
               "Level Select"
               (fn []
                 (reset! state/state {:screen {:type :load-level-select}})
                 (screen/redraw))
               20
               70)
              (ui/menu-button
               "Source Code"
               (fn open-source []
                 (js/window.open "https://github.com/eutro/semantic-construct"))
               120
               170)])}
   :listeners {:click [ui/on-click]}})

(defmethod screen/draw-screen :load [_]
  (reset! state/state default-state)
  (screen/redraw))

(defmethod screen/draw-screen :main-menu
  [{{:keys [scene]} :screen}]
  (r/plot-scene scene))
