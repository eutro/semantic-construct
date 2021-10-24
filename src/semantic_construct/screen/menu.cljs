(ns semantic-construct.screen.menu
  (:require [clojure.core.async :as a]
            [semantic-construct.screen.screen :as screen]
            [semantic-construct.screen.ui :as ui]
            [semantic-construct.mouse :as mouse]
            [semantic-construct.state :as state]
            [semantic-construct.theme :as theme]
            [semantic-construct.render :as r]
            [semantic-construct.audio :as audio]))

(defn force-load [])

(def logo (js/document.getElementById "logo"))
(def default-state
  {:screen {:type :main-menu
            :scene
            (r/->Scene
             [(r/obj-with-bindings r/fill :fillStyle (:background theme/theme))
              (-> (r/sprite-object logo)
                  (r/scale 2.5)
                  (r/translate 0 10)
                  (r/center :x))
              (-> (ui/menu-button
                   "Level Select"
                   (fn []
                     (reset! state/state {:screen {:type :load-level-select}})
                     (screen/redraw))
                   0 240)
                  (r/center :x))
              (-> (ui/menu-button
                   "Source Code"
                   (fn []
                     (js/window.open "https://github.com/eutro/semantic-construct"))
                   0 340)
                  (r/center :x))
              (-> (ui/menu-button
                   "Toggle Music"
                   (fn []
                     (some-> (audio/toggle-track! audio/loop1)
                             .then
                             (.catch (fn [e] (js/alert (str "Can't play music: " e))))))
                   0 440)
                  (r/center :x))])}
   :listeners {:click [ui/on-click]}})

(defonce start-music
  (do
    (some-> (audio/toggle-track! audio/loop1) .then)
    true))

(defmethod screen/draw-screen :load [_]
  (reset! state/state default-state)
  (screen/redraw))

(defmethod screen/draw-screen :main-menu
  [{{:keys [scene]} :screen}]
  (r/plot-scene scene))
