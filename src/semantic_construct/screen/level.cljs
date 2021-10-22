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
  (-> (r/text text)
      (r/obj-with-bindings
       :font "20px sans"
       :fillStyle "white")
      (r/transform (r/translation 10 10))))

(def levels
  [(let [engine
         (eng/features->engine
          feature/TheGame
          feature/Button)]
     (map->Level
      {:engine engine
       :game
       (-> (game/new-game)
           (game/add-init-rules
            ["there" "is" "a" "button"]
            ["when" "the" "button" "is" "pressed" "," "win"])
           (eng/on-change engine))
       :base-scene
       (update base-scene
               :objects
               into
               [(textbox "Do you see those? Those words over there.")])}))])

(defn load-level [id]
  (nth levels id))

(defn gen-pos []
  (Math/round (rand 1600000)))

(defn fit-pos [pos]
  (let [factor (/ 3 4)
        w (.-width r/canvas)
        h (.-height r/canvas)
        nx (+ (/ w 16) (mod pos (* factor w)))
        loops-around (quot pos (* factor))
        ny (+ (/ h 16) (mod (* loops-around 20) (* factor h)))]
    [nx ny]))

(defn set-pos [game id pos]
  (assoc-in game [:properties :intrinsics id :pos] pos))

(defn pos-for! [*game id]
  (or (-> @*game :properties :intrinsics (get id) :pos)
      (let [pos (gen-pos)]
        (swap! *game set-pos id pos)
        pos)))

(defn objects-for [*game id]
  (let [props (-> @*game :properties :id-to-props (get id))
        [x y] (fit-pos (pos-for! *game id))
        translate-to-pos (fn [obj] (r/transform obj (r/translation x y)))]
    (->
     (case (:type props)
       :button (-> (r/rect #(r/->BB 0 0 100 100))
                   (r/obj-with-bindings
                    :fillStyle "red"))
       :word (-> (r/text (:value props))
                 (r/obj-with-bindings
                  :font "20px sans"
                  :fillStyle "white"))
       :rule (-> (r/text ";")
                 (r/obj-with-bindings
                  :font "20px sans"
                  :fillStyle "red"))
       :victory (-> (r/text (if (:had props)
                              "victory (had)"
                              "victory (not yet had)"))
                    (r/obj-with-bindings
                     :font "40px sans"
                     :fillStyle "yellow"))

       (throw (ex-info "No renderer for type" {:type (:type props)})))
     translate-to-pos
     (assoc :id id))))

(defn text-width [text]
  (-> r/ctx (.measureText text) .-width))

(defn place-rules [game]
  (reduce
   (fn [game rule-id]
     (let [{:keys [last-words word-ids]
            :as rule-intrinsics}
           (-> game :properties :intrinsics (get rule-id))
           widths (map text-width last-words)
           gap-width 0
           *game (atom game)
           word-poses
           (next
            (reverse
             (reductions
              -
              (pos-for! *game rule-id)
              (eduction (interpose gap-width)
                        (partition-all 2)
                        (map (partial apply +))
                        (reverse widths)))))
           game @*game]
       (reduce
        (fn [game [pos word-id]]
          (set-pos game word-id pos))
        game
        (map vector word-poses word-ids))))
   game
   (-> game :properties :prop-pair-to-ids (get [:type :rule]))))

(defn set-scene! []
  (let [{:keys [base-scene game]
         {game-objects :objects} :game
         :as level} (-> @state/state :screen :level)
        game (place-rules game)
        *game (atom game)
        scene
        (update base-scene
                :objects
                into
                (comp (map (partial objects-for *game))
                      (filter identity))
                game-objects)]
    (swap! state/state
           update
           :screen
           assoc
           :scene scene
           :level (assoc level :game @*game))))

(defn on-click [evt]
  (let [clicked
        (into (sorted-set)
              (comp
               (map :id)
               (filter identity))
              (-> @state/state :screen :scene r/under-mouse))]
    (when (seq clicked)
      (swap! state/state update-in
             [:screen :level :game]
             (fn [game]
               (reduce
                (fn [game id]
                  (eng/dispatch-event game :click {:target id}))
                game
                clicked)))
      (set-scene!))))

(defn level-state [id]
  (let [level (load-level id)]
    {:screen {:type :level
              :level level}
     :listeners {:resize [set-scene!]
                 :click [on-click]}}))

(defmethod screen/draw-screen :load-level
  [{{:keys [id]} :screen}]
  (reset! state/state (level-state id))
  (set-scene!)
  (screen/redraw))

(defmethod screen/draw-screen :level
  [{{:keys [level scene]} :screen}]
  (r/plot-scene scene))
