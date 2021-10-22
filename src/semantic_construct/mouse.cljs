(ns semantic-construct.mouse)

(def mouse (atom [0 0]))
(defn update-mouse [evt]
  (reset! mouse [(.-pageX evt) (.-pageY evt)]))

(defonce initialized
  (do
    (set! js/document.onmousemove update-mouse)
    true))
