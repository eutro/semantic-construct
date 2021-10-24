(ns semantic-construct.theme)

(def theme
  {:background "#1f1f1f"
   :button {:normal "#bb4400"
            :text {:colour "#ffffff"
                   :font "50px sans"}}
   :game
   {:rule {:word-colour "#ffffff"
           :rule-colour "#ff0000"
           :hint-colour "#00ff00"
           :nohint-colour "#882222"
           :font "40px sans"}
    :square {:colour "#ff0000"}
    :circle {:colour "#00ff00"}
    :triangle {:colour "#0000ff"}
    :victory {:colour "#ffff00"
              :not-had-colour "#ff0000"
              :font "40px sans"}
    :explanatory-text {:font "20px sans"
                       :colour "#ffffff"}}})
