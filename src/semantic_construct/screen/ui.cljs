(ns semantic-construct.screen.ui
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
