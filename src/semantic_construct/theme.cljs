(ns semantic-construct.theme)

(def theme
  {:background "#2f2f2f"
   :button {:normal "#00ff00"
            :hover "#008800"
            :text {:colour "#ffffff"
                   :font "50px sans"}}
   :game
   {:rule {:word-colour "#ffffff"
           :rule-colour "#ff0000"
           :hint-colour "#00ff00"
           :nohint-colour "#882222"
           :font "40px sans"}
    :button {:colour "#ff0000"}
    :victory {:colour "#ffff00"
              :not-had-colour "#ff0000"
              :font "40px sans"}
    :explanatory-text {:font "20px sans"
                       :colour "#ffffff"}}})

(def sprites
  {:back-button [0 0 32 32]})
