(ns semantic-construct.screen.screen
  (:require [semantic-construct.state :as state]))

(defmulti draw-screen
  (fn [{{key :type} :screen} & _] key))

(defn redraw []
  (draw-screen @state/state))
