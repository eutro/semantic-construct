(ns semantic-construct.game.engine
  #?(:cljs (:require-macros [semantic-construct.game.engine :refer [with-engine]]))
  (:require [semantic-construct.parser.evaluator :as ev]
            [semantic-construct.parser.atn :as atn]
            [semantic-construct.game.feature :as f]
            [semantic-construct.game.state :as s]
            [clojure.core.match :as m]))

(defn compile-action
  "Compile an action from a parse into a function."
  [action]
  (m/match action
    {:type :win}
    (fn [game & _] (s/conj-object game {:type :victory, :had true}))

    :else (throw (ex-info "Unrecognised action" {:action action}))))

(defn add-listener
  [game event listener]
  (update-in game [:listeners event] (fnil conj #{}) listener))

(defn remove-listener
  [game event listener]
  (update-in game [:listeners event] disj listener))

(defn listener-apply [event listener]
  (fn [game]
    [#(remove-listener % :click listener)
     (add-listener game :click listener)]))

(defn parse->apply
  "Takes a game a list of parses and yields an application function, or nil.

  The application takes the game state, and yields a vector of the form
  [unapply game]
  where unapply is a unary function reverses the application on the game state."
  [parse]
  (when-let [[single-parse & ambiguous] (seq parse)]
    (when-not ambiguous
      (m/match single-parse
        {:type :rule, :rule rule}
        (m/match rule
          {:type :add, :thing props}
          (fn [game]
            (let [[game id] (s/conj-object-for-id game props)]
              [#(s/disj-object % id) game]))

          {:type :when, :event event, :action action}
          (let [action-fn (compile-action action)]
            (m/match event
              {:type :click, :receiver target}
              (listener-apply
               :click
               (fn [game {evt-target :target}]
                 (if (= evt-target target)
                   (action-fn game {:target target})
                   game)))

              :else (throw (ex-info "Unrecognised event" {:event event}))))

          :else (throw (ex-info "Unrecognised rule" {:rule rule})))

        :else (throw (ex-info "Unrecognised parse" {:parse single-parse}))))))

(defn rule->word-ids [game rule-id]
  (let [rule-words-unsorted (-> game :properties :prop-pair-to-ids (get [:rule rule-id]))
        get-props (-> game :properties :id-to-props)
        word-ids (vec (sort-by (comp :index get-props) rule-words-unsorted))]
    word-ids))

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
      (let [game
            (reduce ;; unapply all in reverse order first
             (fn [game rule-id]
               ((-> game :properties :intrinsics
                    (get rule-id) :unapply (or identity))
                game))
             game (reverse rule-ids))
            game
            (reduce
             (fn [game rule-id]
               (binding [ev/*mapped-syms* (assoc ev/*mapped-syms* 'GAME game)]
                 (let [{:keys [last-words last-pparse unapply]
                        :as rule-intrinsics}
                       (-> game :properties :intrinsics (get rule-id))
                       word-ids (rule->word-ids game rule-id)
                       rule-words (mapv (comp :value get-props) word-ids)
                       new-pparse (atn/pparse atn rule-words)
                       hints (into #{} (atn/suggest atn new-pparse))
                       apply-rule (parse->apply (atn/finish-parse new-pparse))
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
             rule-ids)]
        game))))

(defn dispatch-event [game event payload]
  (reduce (fn [game listener] (listener game payload))
          game
          (-> game :listeners (get event))))

(defrecord Engine [atn vars])

(defn call-with-engine [engine f]
  (binding [ev/*mapped-syms* (:vars engine)]
    (f)))

(defmacro with-engine [engine & body]
  `(call-with-engine ~engine (fn [] ~@body)))

(defn on-change [game engine]
  (with-engine engine
    (-> game
        (reparse-rules (:atn engine))
        (dispatch-event :tick nil))))

(defn merge-vars! [vars defs]
  (reduce (fn [vars [key [merge-fn value]]]
            (if-let [old-val (get vars key)]
              (assoc! vars key (merge-fn old-val value))
              (assoc! vars key value)))
          vars
          defs))

(defn features->engine [& features]
  (map->Engine
   {:atn (into {} (comp (map :atn) cat) features)
    :vars (persistent!
           (transduce
            (map :defs)
            (completing merge-vars!)
            (transient ev/*mapped-syms*)
            features))}))

(comment
  (let [engine (features->engine f/TheGame f/Button)]
    (with-engine engine
      (-> (s/new-game)
          (s/add-init-rules ["there" "is" "a" "button"]
                            ["when" "the" "button" "is" "pressed" "," "win"])
          (on-change engine)
          (s/disj-object 3)
          (on-change engine)
          (dispatch-event :click {:target 13}))))
  ;
  )
