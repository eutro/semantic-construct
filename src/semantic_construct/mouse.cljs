(ns semantic-construct.mouse)

(def mouse (atom [0 0]))
(defn update-mouse [evt]
  (when evt (reset! mouse [(.-pageX evt) (.-pageY evt)])))

(defn unwrap-touch [evt]
  (-> evt .-touches first))

(defonce initialized
  (do
    (let [unwrapped (comp update-mouse unwrap-touch)]
      (doseq [[evt listener]
              [["mousedown" update-mouse]
               ["mousemove" update-mouse]
               ["mouseup" update-mouse]
               ["touchstart" unwrapped]
               ["touchmove" unwrapped]
               ["touchend" unwrapped]]]
        (js/document.addEventListener evt listener)))
    true))
