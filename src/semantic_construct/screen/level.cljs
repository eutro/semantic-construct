(ns semantic-construct.screen.level
  (:require [semantic-construct.screen.screen :as screen]
            [semantic-construct.mouse :as mouse]
            [semantic-construct.state :as state]
            [semantic-construct.theme :as theme]
            [semantic-construct.render :as r]
            [semantic-construct.game.engine :as eng]
            [semantic-construct.game.state :as game]
            [semantic-construct.game.feature :as feature]))

(defrecord Level [game engine base-scene])

(def base-scene
  (r/->Scene
   [(r/obj-with-bindings r/fill :fillStyle (:background theme/theme))]))

(defn textbox [text]
  (let [text-obj (r/text text)
        rect-bb (-> text-obj :bb (r/enlarge 5))]
    [(r/rect rect-bb)
     (r/stroke-rect rect-bb)
     text-obj]))

(defn textboxes [& texts]
  )

(def levels
  [(let [engine
         (eng/features->engine
          feature/TheGame
          feature/Button)]
     (map->Level
      {:engine engine
       :game
       (-> (game/new-game)
           (game/add-init-words
            "a" "button"
            "the" "button" "is" "pressed" "," "win")
           (game/add-init-rules ["there" "is"] ["when"])
           (eng/on-change engine))
       :base-scene
       (update base-scene
               :objects
               into
               (textboxes
                "Do you see those?"
                "Those words over there."
                "I think you can make sentences with them."))}))])

(defn load-level [id]
  (nth levels id))

(defn sprites-for [game id]
  (let [props (-> game :properties :id-to-props (get id))]
    (case (:type props)
      (:word :rule) nil ;; words and rules are rendered separately

      (throw (ex-info "No renderer for type" {:type (:type props)})))))

(defn fresh-scene [level]
  (let [{:keys [base-scene game]
         {game-objects :objects} :game} level]
    (update base-scene
            :objects
            into
            (comp (map (partial sprites-for game)) cat)
            game-objects)))

(defn level-state [id]
  (let [level (load-level id)]
    {:screen {:type :level
              :level level
              :scene (fresh-scene level)}}))

(defmethod screen/draw-screen :load-level
  [{{:keys [id]} :screen}]
  (reset! state/state (level-state id))
  (screen/redraw))

(defmethod screen/draw-screen :level
  [{{:keys [level scene]} :screen}]
  (r/plot-scene scene))
