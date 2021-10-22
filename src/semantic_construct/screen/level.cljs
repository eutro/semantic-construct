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
      (r/transform (r/translation 10 30))))

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
  (Math/round (rand 160000)))

(defn unfit-pos [[x y]]
  (let [unfit (fn [v cap]
                (- v (/ cap 16)))
        w (.-width r/canvas)
        h (.-height r/canvas)]
    [(unfit x w) (unfit y h)]))

(defn fit-pos [pos]
  (let [factor (/ 3 4)
        fit (fn [v cap]
              (+ (/ cap 16) (rem v (* factor cap))))
        w (.-width r/canvas)
        h (.-height r/canvas)
        [x y] (if (vector? pos)
                pos
                [pos (* 10 (quot pos (* factor w)))])
        [x y] [(fit x w) (fit y h)]]
    [x y]))

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
                    :fillStyle (-> theme/theme :game :button :colour)))
       :word (-> (r/text (:value props))
                 (r/obj-with-bindings
                  :font (-> theme/theme :game :rule :font)
                  :fillStyle (-> theme/theme :game :rule :word-colour)))
       :rule (-> (r/text ";")
                 (r/obj-with-bindings
                  :font (-> theme/theme :game :rule :font)
                  :fillStyle (-> theme/theme :game :rule :rule-colour)))
       :victory (-> (r/text (if (:had props)
                              "victory (had)"
                              "victory (not yet had)"))
                    (r/obj-with-bindings
                     :font (-> theme/theme :game :victory :font)
                     :fillStyle (-> theme/theme :game :victory :colour)))

       (throw (ex-info "No renderer for type" {:type (:type props)})))
     translate-to-pos
     (assoc :id id))))

(defn text-width [text]
  (-> r/ctx (.measureText text) .-width))

(def no-space? (partial contains? #{nil "," ":" ";"}))

(defn place-rules [game]
  (let [counter (atom 0)]
    (reduce
     (fn [game rule-id]
       (r/with-ctx-bindings [font (-> theme/theme :game :rule :font)]
         (let [{:keys [last-words word-ids]
                :as rule-intrinsics}
               (-> game :properties :intrinsics (get rule-id))
               gap-width (text-width " ")
               widths ((fn widths [words]
                         (lazy-seq
                          (when (seq words)
                            (cons
                             (+ (text-width (first words))
                                (if (no-space? (second words)) 0 gap-width))
                             (widths (next words))))))
                       last-words)
               *game (atom game)
               word-poses (reductions + 0 widths)
               y (* 50 (swap! counter inc))
               game @*game]
           (reduce
            (fn [game [x id]]
              (set-pos game id [x y]))
            game
            (map vector word-poses (concat word-ids [rule-id]))))))
     game
     (-> game :properties :prop-pair-to-ids (get [:type :rule])))))

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
              (-> @state/state :screen :scene r/under-mouse))
        clicked (disj clicked (-> @state/state :screen :just-moved))]
    (when (seq clicked)
      (swap! state/state update-in
             [:screen :level :game]
             (fn [game]
               (reduce
                (fn [game id]
                  ()
                  (eng/dispatch-event game :click {:target id}))
                game
                clicked)))
      (set-scene!))))

(defn on-down [evt]
  (when-some [[{id :id, :as obj}]
              (eduction
               (filter :id)
               (-> @state/state :screen :scene r/under-mouse))]
    (swap! state/state update :screen assoc :held
           (let [[dx dy] (map -
                              (-> @state/state :screen :level :game atom
                                  (pos-for! id)
                                  fit-pos)
                              @mouse/mouse)]
             {:id id, :dx dx, :dy dy})))
  (when (-> @state/state :screen :just-moved)
    (swap! state/state update :screen dissoc :just-moved)))

(defn on-up [evt]
  (swap! state/state update :screen dissoc :held))

(defn on-move [evt]
  (when-some [{:keys [id dx dy]} (-> @state/state :screen :held)]
    (swap! state/state update-in [:screen :level :game]
           set-pos id (unfit-pos (mapv + @mouse/mouse [dx dy])))
    (swap! state/state update :screen assoc :just-moved id)
    (set-scene!)
    (screen/redraw)))

(defn level-state [id]
  (let [level (load-level id)]
    {:screen {:type :level
              :level level}
     :listeners {:resize [set-scene!]
                 :click [on-click]
                 :mousedown [on-down]
                 :mouseup [on-up]
                 :mousemove [on-move]}}))

(defmethod screen/draw-screen :load-level
  [{{:keys [id]} :screen}]
  (reset! state/state (level-state id))
  (set-scene!)
  (screen/redraw))

(defmethod screen/draw-screen :level
  [{{:keys [level scene]} :screen}]
  (r/plot-scene scene))
