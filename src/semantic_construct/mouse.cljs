(ns semantic-construct.mouse)

(def mouse (atom [0 0]))
(defn update-mouse [evt]
  (when evt (reset! mouse [(.-pageX evt) (.-pageY evt)])))

(defn unwrap-touch [evt]
  (-> evt .-touches first))

(defonce initialized
  (do
    (set! js/document.onmousemove update-mouse)
    (set! js/document.ontouchmove (comp update-mouse unwrap-touch))
    true))
