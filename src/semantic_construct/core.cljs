(ns semantic-construct.core
  (:require [cljs.core.async :as a :refer-macros [go]]))

(def grid-size [8 6])
(def tile-size 32)

(def canvas (. js/document (getElementById "canvas")))
(doto canvas
  (-> .-width (set! (* (grid-size 0) tile-size)))
  (-> .-height (set! (* (grid-size 1) tile-size))))

(def ctx (. canvas (getContext "2d")))
(def sprites (. js/document (getElementById "sprites")))

(def objects
  {:tile {:sprite {:x 0, :y 0, :w tile-size, :h tile-size}}
   :button {:sprite {:x (* tile-size 1), :y 0, :w tile-size, :h tile-size}}
   :button-pressed {:sprite {:x (* tile-size 2), :y 0, :w tile-size, :h tile-size}}
   :block {:sprite {:x (* tile-size 3), :y 0, :w tile-size, :h tile-size}}})

(defn offset-sprite [sprite offset]
  (let [{:keys [h]} sprite]
    (update sprite :y + (* h offset))))

(comment
  (offset-sprite (-> objects :tile :sprite) 2))

(def colour->offset
  {:green 0, :red 1, :blue 2, :yellow 3})

(defn colour-sprite [sprite colour]
  (offset-sprite
   sprite
   (or (colour->offset colour)
       (throw (ex-info "No such colour" {:colour colour})))))

(comment
  (colour-sprite (-> objects :tile :sprite) :yellow)
  (colour-sprite (-> objects :tile :sprite) :brown))

(defn object-sprite
  ([object]
   (-> objects
       (get object)
       (or (throw (ex-info "No such object" {:object object})))
       :sprite))
  ([object colour] (colour-sprite (object-sprite object) colour)))

(defn draw-sprite [ctx sprite dx dy]
  (let [{:keys [x y w h]} sprite]
    (.drawImage ctx
                sprites
                x y
                w h
                dx dy
                w h)))

(comment
  (draw-sprite ctx (object-sprite :button :red) 64 40))
