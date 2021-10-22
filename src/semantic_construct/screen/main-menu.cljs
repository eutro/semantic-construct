(ns semantic-construct.screen.main-menu
  (:require [semantic-construct.screen.screen :as screen]
            [semantic-construct.mouse :as mouse]
            [semantic-construct.state :as state]
            [semantic-construct.theme :as theme]
            [semantic-construct.render :as r]))

(defn on-click [evt]
  (->> (-> @state/state :screen :scene r/under-mouse)
       (map :on-click)
       (filter identity)
       (run! #(% evt))))

(defn open-source []
  (js/window.open "https://github.com/eutro/semantic-construct"))

(defn menu-button [text callback x y]
  (let [{{{text-font :font
           text-colour :colour} :text
          button-hover :hover
          button-normal :normal}
         :button} theme/theme
        text-obj
        (r/obj-with-bindings
         (r/text text)
         :font text-font
         :fillStyle text-colour)
        button-obj
        (r/obj-with-bindings
         (r/rect #(r/enlarge (:bb @text-obj) 10))
         :fillStyle button-normal)]
    (-> (r/combine button-obj text-obj)
        (r/transform (-> (js/DOMMatrix.) (.translateSelf x y)))
        (assoc :on-click callback))))

(def default-state
  {:screen {:type :main-menu
            :scene
            (r/->Scene
             [(r/obj-with-bindings r/fill :fillStyle (:background theme/theme))
              (menu-button "Level Select"
                           (fn []
                             (reset! state/state {:screen {:type :load-level, :id 0}})
                             (screen/redraw))
                           20
                           70)
              (menu-button "Source Code" open-source 120 170)])}
   :listeners {:click [on-click]}})

(defmethod screen/draw-screen :load [_]
  (reset! state/state default-state)
  (screen/redraw))

(defmethod screen/draw-screen :main-menu
  [{{:keys [scene]} :screen}]
  (r/plot-scene scene))
