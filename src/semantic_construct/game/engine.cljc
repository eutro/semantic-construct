(ns semantic-construct.game.engine
  (:require [semantic-construct.parser.atn :as atn]
            [semantic-construct.game.feature :as f]
            [semantic-construct.game.state :as s]
            [clojure.core.match :as m]))

(defn compile-action
  "Compile an action from a parse into a function."
  [action]
  (m/match action
    {:type :win}
    (fn [game & _]
      (binding [f/*vars* (assoc f/*vars* :game game)]
        (if (seq (f/prop-intersection [:type :victory] [:had true]))
          game
          (s/conj-object game {:type :victory, :had true}))))

    :else (throw (ex-info "Unrecognised action" {:action action}))))

(defn add-listener
  [game event listener]
  (update-in game [:listeners event] (fnil conj #{}) listener))

(defn remove-listener
  [game event listener]
  (update-in game [:listeners event] disj listener))

(defn listener-apply [event listener]
  (fn reapply [game]
    ;; removing listeners should not undo actions done by it
    [(fn [game] [reapply (remove-listener game event listener)])
     (add-listener game event listener)]))

(defn comp-apply
  ([] nil)
  ([apply] apply)
  ([app1 app2]
   (fn [game]
     (let [[unapply1 game] (app1 game)
           [unapply2 game] (app2 game)]
       [(comp-apply unapply1 unapply2) game])))
  ([app1 app2 app3]
   (fn [game]
     (let [[unapply1 game] (app1 game)
           [unapply2 game] (app2 game)
           [unapply3 game] (app3 game)]
       [(comp-apply unapply1 unapply2 unapply3) game])))
  ([app1 app2 app3 & rest]
   (fn [game]
     (let [[unapplies game]
           (reduce (fn [[unapplies game] app]
                     (let [[unapply game] (app game)]
                       [(conj unapplies unapply) game]))
                   (let [[unapply game] (app1 game)]
                     [[unapply] game])
                   (list* app2 app3 rest))]
       [(apply comp-apply unapplies) game]))))

(defn add-application
  ([visible] (add-application visible nil))
  ([visible intrinsics]
   (fn [game]
     (let [[game id] (s/conj-object-for-id game visible)
           game (if intrinsics
                  (assoc-in game [:properties :intrinsics id] intrinsics)
                  game)]
       [(fn [game]
          (let [{:keys [visible intrinsics]} (s/obj-props game id)]
            [(add-application visible intrinsics)
             (s/disj-object game id)]))
        game]))))

(defn compile-event [event action-fn]
  (m/match event
    {:type :click, :receiver target}
    (listener-apply
     :click
     (fn [game {evt-target :target}]
       (if (binding [f/*vars* (assoc f/*vars* :game game)]
             (target evt-target))
         (action-fn game {:target evt-target})
         game)))

    {:type :tick, :pred pred}
    (listener-apply
     :tick
     (fn [game _]
       (if (binding [f/*vars* (assoc f/*vars* :game game)] (pred))
         (action-fn game {})
         game)))

    :else (throw (ex-info "Unrecognised event" {:event event}))))

(defn parse->apply [parse]
  (m/match parse
    {:type :compose, :composed composed}
    (apply comp-apply (map parse->apply composed))

    {:type :repeat, :count count, :rule rule}
    (apply comp-apply (repeat count (parse->apply rule)))

    {:type :add, :thing props}
    (add-application props)

    {:type :when, :event event, :action action}
    (compile-event event (compile-action action))

    :else (throw (ex-info "Unrecognised rule" {:rule parse}))))

(defn parses->apply
  "Takes a game a list of parses and yields an application function, or nil.

  An application function takes the game state, and yields a vector of the form
  [reversion game]
  Where reversion is another application function that reverses the application.

  In some cases, such a reversion would be destructive, and lose state that has
  since been gained (e.g. deleting an object)
  The reversion function must undo the application function, but the application
  function might not necessarily fully undo the reversion. The reversion function
  therefore returns its own reversion function that has the correct effect."
  [parses]
  (when-let [[single-parse & ambiguous] (seq parses)]
    (when-not ambiguous
      (parse->apply single-parse))))

(defn rule->word-ids [game rule-id]
  (let [rule-words-unsorted (-> game :properties :prop-pair-to-ids (get [:rule rule-id]))
        get-props (-> game :properties :id-to-props)
        word-ids (vec (sort-by (comp :index get-props) rule-words-unsorted))]
    word-ids))

(defn normalise-rule-indices [game rule-id]
  (transduce 
   (map-indexed vector)
   (completing
    (fn [game [i id]]
      (s/assoc-props game id {:index i})))
   game
   (rule->word-ids game rule-id)))

(defn reparse-rules [game atn]
  (let [rule-ids
        (-> game
            :properties
            :prop-pair-to-ids
            (get [:type :rule]))
        get-props (-> game :properties :id-to-props)
        unchanged (every? (partial apply =)
                          (map
                           (juxt
                            (comp :last-words (-> game :properties :intrinsics))
                            (comp (partial mapv (comp :value get-props))
                                  (partial rule->word-ids game)))
                           rule-ids))]
    (if unchanged
      game
      (let [[revl game]
            (reduce ;; unapply all in reverse order first
             (fn [[revl game] rule-id]
               (let [[reapply game]
                     (if-let [unapply (-> game :properties :intrinsics
                                          (get rule-id) :unapply)]
                       (unapply game)
                       [nil game])]
                 [(cons [rule-id reapply] revl) game]))
             [nil game]
             (reverse rule-ids))
            game
            (reduce
             (fn [game [rule-id reapply]]
               (binding [f/*vars* (assoc f/*vars* :game game)]
                 (let [{:keys [last-words last-pparse unapply]
                        :as rule-intrinsics}
                       (-> game :properties :intrinsics (get rule-id))
                       word-ids (rule->word-ids game rule-id)
                       rule-words (mapv (comp :value get-props) word-ids)
                       new-pparse (atn/pparse atn rule-words)
                       hints (into #{} (atn/suggest atn new-pparse))
                       full-parse (atn/finish-parse new-pparse)
                       apply-rule (if (and reapply (= full-parse (atn/finish-parse last-pparse)))
                                    reapply ;; faithfully undo the unapplication :)
                                    (parses->apply full-parse))
                       [unapply-rule game] (if apply-rule (apply-rule game) [nil game])
                       game (assoc-in
                             game
                             [:properties :intrinsics rule-id]
                             (assoc rule-intrinsics
                                    :last-words rule-words
                                    :word-ids word-ids
                                    :last-pparse new-pparse
                                    :hints hints
                                    :unapply unapply-rule
                                    :applied (boolean apply-rule)))]
                   game)))
             game
             revl)
            game (reduce normalise-rule-indices game rule-ids)]
        game))))

(defn dispatch-event [game event payload]
  (reduce (fn [game listener] (listener game payload))
          game
          (-> game :listeners (get event))))

(defn on-change [game]
  (-> game
      (reparse-rules f/atn)
      (dispatch-event :tick nil)))

(comment
  (-> (s/new-game)
      (s/add-init-rules ["there" "is" "a" "button"]
                        ["when" "the" "button" "is" "pressed" "," "win"])
      (on-change engine)
      (dispatch-event :click {:target 13}))
  ;
  )
