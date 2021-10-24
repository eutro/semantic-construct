(ns semantic-construct.core
  (:require [cljs.core.async :as a :refer-macros [go]]
            [semantic-construct.render :as r]
            [semantic-construct.state :as state]
            [semantic-construct.screen.screen :as scr]
            [semantic-construct.screen.menu :as menu]
            [semantic-construct.screen.level :as level]))

(menu/force-load)
(level/force-load)

(defn resize-canvas-to-window []
  (doto r/canvas
    (-> .-width (set! js/window.innerWidth))
    (-> .-height (set! js/window.innerHeight)))
  (state/call-with-escape-base scr/redraw))

(defn animate []
  (try
    (state/call-with-escape-base scr/redraw)
    (finally
      (js/window.requestAnimationFrame animate))))

(defn event-dispatcher [name]
  (let [kw (keyword name)]
    (fn [evt]
      (state/call-with-escape-base
       (fn []
         (run! #(% evt) (kw (:listeners @state/state))))))))

(defn add-event-dispatcher
  ([name] (add-event-dispatcher name name))
  ([evt name] (js/window.addEventListener evt (event-dispatcher name))))

(defonce initialized
  (do
    (resize-canvas-to-window)
    (js/window.addEventListener "resize" resize-canvas-to-window false)
    (add-event-dispatcher "click")
    (add-event-dispatcher "resize")
    (add-event-dispatcher "mousedown")
    (add-event-dispatcher "mouseup")
    (add-event-dispatcher "mousemove")
    (add-event-dispatcher "touchstart" "mousedown")
    (add-event-dispatcher "touchmove" "mousemove")
    (add-event-dispatcher "touchend" "mouseup")
    (add-event-dispatcher "keydown")
    (-> js/document .-body .-onload
        (set! #(js/window.requestAnimationFrame animate)))
    true))
