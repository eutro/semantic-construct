(ns semantic-construct.screen.level
  (:require [semantic-construct.screen.screen :as screen]
            [semantic-construct.screen.ui :as ui]
            [semantic-construct.mouse :as mouse]
            [semantic-construct.state :as state]
            [semantic-construct.theme :as theme]
            [semantic-construct.render :as r]
            [semantic-construct.game.engine :as eng]
            [semantic-construct.game.state :as game]
            [semantic-construct.game.feature :as feature]
            [clojure.core.match :as m]))

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
               [(textbox "The rules are self-explanatory:")])}))

   (let [engine
         (eng/features->engine
          feature/TheGame
          feature/Natural
          feature/Win
          feature/Button)]
     (map->Level
      {:engine engine
       :game
       (-> (game/new-game)
           (game/add-init-rules
            ["there"]
            ["win" "when"])
           (game/add-init-words
            "are" "two" "a" "buttons"
            "button" "button" "the" "is" "pressed")
           (eng/on-change engine))
       :base-scene
       (update base-scene
               :objects
               into
               [(textbox "Feel free to change the rules, though.")])}))

   (let [engine
         (eng/features->engine
          feature/TheGame
          feature/Natural
          feature/Win
          feature/Button)]
     (map->Level
      {:engine engine
       :game
       (-> (game/new-game)
           (game/add-init-rules
            ["there" "is"]
            ["win" "when"])
           (game/add-init-words
            "are" "two" "a" "\"" "\"" "'" "'" "an" "pressed")
           (eng/on-change engine))
       :base-scene
       (update base-scene
               :objects
               into
               [(textbox "Quotation marks seem handy...")])}))])

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
  (let [id-to-props (-> @*game :properties :id-to-props)
        props (id-to-props id)
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
       :rule (let [held (-> @state/state :screen :held :id)
                   word (and held
                             (-> held id-to-props :type (= :word))
                             (-> held id-to-props :value))
                   intrinsics (-> @*game :properties :intrinsics (get id))
                   style (cond
                           (not word) :rule-colour
                           (contains? (:hints intrinsics) word) :hint-colour
                           :else :nohint-colour)]
               (-> (r/text (if (:applied intrinsics) ";" "..."))
                   (r/obj-with-bindings
                    :font (-> theme/theme :game :rule :font)
                    :fillStyle (-> theme/theme :game :rule style))))
       :victory (-> (r/text (if (:had props)
                              "victory (had)"
                              "victory (not had)"))
                    (r/obj-with-bindings
                     :font (-> theme/theme :game :victory :font)
                     :fillStyle (-> theme/theme :game :victory
                                    ((if (:had props)
                                       :colour
                                       :not-had-colour)))))

       (throw (ex-info "No renderer for type" {:type (:type props)})))
     translate-to-pos
     (assoc :id id))))

(defn text-width [text]
  (-> r/ctx (.measureText text) .-width))

(defn no-space-after? [next-word]
  (contains? #{nil "," ":" ";"} next-word))

(defn add-spaces [words]
  (loop [out []
         words words
         quoting []]
    (if-let [[word next-word] (seq words)]
      (m/match [[word next-word]]
        [([_ (:or "\"" "'")] :guard #(= next-word (peek quoting)))]
        (recur (conj out word) (next words) quoting)
        [[(:or "\"" "'") _]]
        (if (= word (peek quoting))
          (recur (conj out (if (no-space-after? next-word) word (str word " ")))
                 (next words)
                 (pop quoting))
          (recur (conj out word) (next words) (conj quoting word)))
        [[_ _] :guard #(no-space-after? next-word)]
        (recur (conj out word) (next words) quoting)
        :else (recur (conj out (str word " ")) (next words) quoting))
      out)))

(defn place-rules [game]
  (let [counter (atom 0)]
    (reduce
     (fn [game rule-id]
       (r/with-ctx-bindings [font (-> theme/theme :game :rule :font)]
         (let [{:keys [last-words word-ids]
                :as rule-intrinsics}
               (-> game :properties :intrinsics (get rule-id))
               *game (atom game)
               widths (map text-width (add-spaces last-words))
               word-poses (reductions + 0 widths)
               y (* 50 (swap! counter inc))
               held (-> @state/state :screen :held :id)
               game @*game]
           (reduce
            (fn [game [x id]]
              (if (= held id)
                game
                (set-pos game id [x y])))
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

(defn level-changes [level]
  (update level :game eng/on-change (:engine level)))

(defn check-changes! []
  (swap! state/state update-in [:screen :level] level-changes)
  (set-scene!))

(defn on-click [evt]
  (let [clicked
        (into (sorted-set)
              (comp
               (map :id)
               (filter identity))
              (-> @state/state :screen :scene r/under-mouse))
        clicked (disj clicked (-> @state/state :screen :just-moved))]
    (when-some [[clicked-id] (seq clicked)]
      (eng/with-engine (-> @state/state :screen :level :engine)
        (swap! state/state update-in
               [:screen :level :game]
               eng/dispatch-event
               :click
               {:target clicked-id}))
      (check-changes!))))

(defn on-down [evt]
  (when-some [[{id :id, :as obj}]
              (eduction
               (filter :id)
               (-> @state/state :screen :scene r/under-mouse))]
    (swap! state/state update :screen assoc :held
           (let [[ox oy] (map -
                              (-> @state/state :screen :level :game atom
                                  (pos-for! id)
                                  fit-pos)
                              @mouse/mouse)
                 [sx sy] @mouse/mouse]
             {:id id, :ox ox, :oy oy, :sx sx, :sy sy})))
  (when (-> @state/state :screen :just-moved)
    (swap! state/state update :screen dissoc :just-moved)))

(defn square [x]
  (* x x))

(defn eucl-sq [& xs]
  (transduce (map square) + xs))

(defn on-up [evt]
  (when-some [{:keys [sx sy id]} (-> @state/state :screen :held)]
    (swap! state/state update :screen dissoc :held)
    (let [game (-> @state/state :screen :level :game)
          dist-sq (apply eucl-sq (map - [sx sy] @mouse/mouse))]
      (when (<= 100 dist-sq)
        (swap! state/state update :screen assoc :just-moved id)
        (let [game
              (eng/with-engine (-> @state/state :screen :level :engine)
                (eng/dispatch-event game :move {:target id}))
              id-to-props (-> game :properties :id-to-props)
              props (id-to-props id)
              scene (-> @state/state :screen :scene)
              game
              (if-not (and (:rule props) (<= 400 dist-sq))
                game
                ;; pop from the rule
                (let [game (game/assoc-props game id {:rule nil, :index nil})
                      rule-id (:rule props)
                      left-in-rule (-> game :properties :prop-pair-to-ids (get [:rule rule-id]))
                      game (if (= (count left-in-rule) (:index props))
                             game ;; only the last word taken off, no need to shuffle
                             (eng/normalise-rule-indices game rule-id)
                             ;; move all the existing words around
                             )]
                  game))
              id-to-props (-> game :properties :id-to-props)
              props (id-to-props id)
              game
              ;; maybe add to rule
              (if-let [{:keys [rule index]}
                       (and
                        (= :word (:type props))
                        (not (:rule props))
                        (transduce
                         (comp
                          (filter :id)
                          (map (fn [{:keys [id] :as obj}]
                                 (let [{:keys [type rule index]} (id-to-props id)]
                                   (cond
                                     (= :rule type)
                                     {:obj obj, :rule id, :index ##Inf}
                                     (and (= :word type) rule)
                                     {:obj obj, :rule rule, :index index}
                                     :else nil))))
                          (filter identity)
                          (let [this-bb (:bb @(first (filter (comp #{id} :id) (:objects scene))))]
                            (filter #(r/bb-intersects? this-bb (-> % :obj deref :bb)))))
                         (completing
                          (fn [x y]
                            (cond
                              (not x) y
                              (not= (:rule x) (:rule y)) (min-key :rule x y)
                              :else (max-key :index x y))))
                         nil
                         (:objects scene)))]
                (let [in-rule (-> game :properties :prop-pair-to-ids (get [:rule rule]))
                      game (game/assoc-props game id {:rule rule})
                      target-index (if (infinite? index) (count in-rule) index)
                      game (if (infinite? index)
                             game
                             (reduce (fn [game id]
                                       (let [index (:index (id-to-props id))]
                                         (if (>= index target-index)
                                           (game/assoc-props game id {:index (inc index)})
                                           game)))
                                     game in-rule))
                      game (game/assoc-props game id {:index target-index})]
                  game)
                game)]
          (swap! state/state assoc-in [:screen :level :game] game))
        (check-changes!)
        (screen/redraw)))))

(defn on-move [evt]
  (when-some [{:keys [id ox oy]} (-> @state/state :screen :held)]
    (swap! state/state update-in [:screen :level :game]
           set-pos id (unfit-pos (mapv + @mouse/mouse [ox oy])))
    (set-scene!)
    (screen/redraw)))

(defn load-level [id]
  (nth levels id))

(defn level-state [id]
  (let [level (load-level id)]
    {:screen {:type :level
              :level level}
     :listeners {:resize [set-scene!]
                 :click [on-click]
                 :mousedown [on-down]
                 :mouseup [on-up]
                 :mousemove [on-move]}}))

(defn set-level! [id]
  (reset! state/state (level-state id))
  (set-scene!)
  (screen/redraw))

(defmethod screen/draw-screen :load-level-select
  [_]
  (reset!
   state/state
   {:screen {:type :level-select
             :scene (r/->Scene
                     (into [(r/obj-with-bindings r/fill :fillStyle (:background theme/theme))]
                           (map-indexed
                            (fn [i lvl]
                              (ui/menu-button
                               (str i)
                               (fn [] (set-level! i))
                               (* (inc (quot i 3)) 60)
                               (* (inc (mod i 3)) 60))))
                           levels))}
    :listeners {:click [ui/on-click]}})
  (screen/redraw))

(defmethod screen/draw-screen :level-select
  [{{:keys [level scene]} :screen}]
  (r/plot-scene scene))

(defmethod screen/draw-screen :level
  [{{:keys [level scene]} :screen}]
  (r/plot-scene scene))
