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

(defn menu-button
  ([text callback x y] (menu-button text callback nil nil x y))
  ([text callback w h x y]
   (let [{{{text-font :font
            text-colour :colour} :text
           button-normal :normal}
          :button} theme/theme
         text-obj
         (r/obj-with-bindings
          (r/text text)
          :font text-font
          :fillStyle text-colour)
         button-obj
         (r/obj-with-bindings
          (r/rect (if (and w h)
                    #(let [{:keys [minx miny maxx maxy]} (:bb @text-obj)
                           sumx (+ minx maxx)
                           sumy (+ miny maxy)]
                       (r/->BB
                        (/ (- sumx w) 2)
                        (/ (- sumy h) 2)
                        (/ (+ sumx w) 2)
                        (/ (+ sumy h) 2)))
                    #(r/enlarge (:bb @text-obj) 10)))
          :font text-font
          :fillStyle button-normal)]
     (-> (r/combine button-obj text-obj)
         (r/transform (r/translation x y))
         (assoc :on-click callback)))))
