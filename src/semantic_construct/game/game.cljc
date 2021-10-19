(ns semantic-construct.game.game
  (:require [clojure.set :as set]
            [semantic-construct.game.objects :as o]))

(defn apply-rule [game rule]
  ((:aply rule) game))

(defn unapply-rule [game rule]
  ((:unapply rule) game))

(defn update-rules [{old-rules :rules
                     :as old-game}
                    {new-rules :rules
                     :as new-game}]
  (cond
    (identical? old-rules new-rules)
    new-game

    :else
    (let [added-rules (set/difference new-rules old-rules)
          removed-rules (set/difference old-rules new-rules)]
      (as-> new-game $
        (reduce unapply-rule $ removed-rules)
        (reduce apply-rule $ added-rules)))))

(defn act [game action payload]
  (update-rules
   game
   (reduce (fn [game listener]
             (listener game action payload))
           game
           (get-in game [:events action]))))
