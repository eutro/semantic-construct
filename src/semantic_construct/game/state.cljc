(ns semantic-construct.game.state
  (:require [semantic-construct.parser.atn :as atn]
            [clojure.core.match :as m]))

(defrecord GameState
    [objects
     properties
     listeners ;; (mapof event (setof (game payload . -> . game)))
     ])
(defrecord GameProperties
    [id-to-props
     prop-pair-to-ids
     prop-to-ids
     intrinsics ;; special internal properties that don't need to be tracked
     ]
  )

(def conj-or-ss
  (fnil conj (sorted-set)))

(defn assoc-props [game id props+values]
  (when-not (contains? (:objects game) id)
    (throw (ex-info "Object with id does not exist" {:game game, :id id})))
  (update
   game :properties
   (fn [props]
     (reduce
      (fn [props [prop value]]
        (let [id-to-props (:id-to-props props)
              existing-pair (some-> id-to-props (get id) (find prop))
              new-pair [prop value]]
          (if (= new-pair existing-pair)
            props
            (let [props
                  (if value
                    (-> props
                        (update :id-to-props update id assoc prop value)
                        (update :prop-pair-to-ids update new-pair conj-or-ss id))
                    (-> props
                        (update :id-to-props update id dissoc prop)
                        (update :prop-to-ids update prop disj id)))
                  props
                  (if existing-pair
                    (update props :prop-pair-to-ids update existing-pair disj id)
                    (update props :prop-to-ids update prop conj-or-ss id))]
              props))))
      props
      props+values))))

(defn conj-object-for-id [game props]
  (let [id (if-let [hi-id (first (rseq (:objects game)))]
             (inc hi-id)
             0)
        game (update game :objects conj id)
        game (assoc-props game id props)]
    [game id]))

(defn conj-object [game props]
  (first (conj-object-for-id game props)))

(defn obj-props [game id]
  (let [props (:properties game)]
    {:visible (get (:id-to-props props) id)
     :intrinsics (get (:intrinsics props) id)}))

(defn obj-set-props [game id props]
  (-> game
      (assoc-props id (:visible props))
      (update-in [:intrinsics id] (:intrinsics props))))

(defn disj-object [game id]
  (-> game
      (assoc-props id (into {}
                            (map (fn [[k v]] [k nil]))
                            (-> game :properties :id-to-props (get id))))
      (update :objects disj id)
      (update-in [:properties :id-to-props] dissoc id)
      (update-in [:properties :intrinsics] dissoc id)))

(defn new-game
  "Creates a new GameState with the given init objects.

  Returns a map of the following form:

  #GameState
  {:objects #{...}
   :listeners {...}
   :properties
   #GameProperties
   {:id-to-props {...}
    :prop-to-ids {...}
    :prop-pair-to-ids {...}
    :intrinsics {...}}}
  "
  [& objects]
  (reduce conj-object
          (map->GameState
           {:objects (sorted-set) ;; set of ids
            :listeners {}
            :properties (map->GameProperties
                         {:id-to-props (sorted-map) ;; sorted prints nicer
                          :prop-to-ids {}
                          :prop-pair-to-ids (sorted-map)
                          :intrinsics {}})})
          objects))

(defn add-init-words [game & words]
  (transduce (map (fn [word]
                    {:type :word
                     :value word}))
             (completing conj-object)
             game
             words))

(defn add-init-rules [game & rules]
  (reduce
   (fn [game rule]
     (let [[game rule-id] (conj-object-for-id game {:type :rule})
           game (transduce (map-indexed
                            (fn [i word]
                              {:type :word
                               :value word
                               :rule rule-id
                               :index i}))
                           (completing conj-object)
                           game
                           rule)]
       game))
   game
   rules))
