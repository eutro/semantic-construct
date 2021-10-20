(ns semantic-construct.game.objects
  "Contains all the objects that exist in a game."
  (:require #?(:cljs [semantic-construct.game.objects :refer-macros [defobject]])
            [semantic-construct.parser.sentence :as sentence]
            [semantic-construct.parser.evaluator :as ev]
            [clojure.set :as set]))

(defrecord Game [objects properties])
(defrecord GameObject [name atn defs])

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
                        (update :id-to-props dissoc id)
                        (update :props-to-ids update prop disj id)))
                  props
                  (if existing-pair
                    (update props :prop-pair-to-ids update existing-pair disj id)
                    (update props :prop-to-ids update prop conj-or-ss id))]
              props))))
      props
      props+values))))

(defn conj-object [game props]
  (let [id (if-let [hi-id (first (rseq (:objects game)))]
             (inc' hi-id)
             0)
        game (update game :objects conj id)
        game (assoc-props game id props)]
    game))

(defn new-game [& objects]
  (reduce conj-object
          (map->Game
           {:objects (sorted-set) ;; set of ids
            :properties {:id-to-props (sorted-map)
                         :prop-to-ids (sorted-map)
                         :prop-pair-to-ids (sorted-map)}})
          objects))

(comment
  (new-game
   {:type :word, :value "there", :rule 11, :index 0}
   {:type :word, :value "is", :rule 11, :index 1}
   {:type :word, :value "a", :rule 11, :index 2}
   {:type :word, :value "button", :rule 11, :index 3}
   {:type :word, :value "when", :rule 12, :index 0}
   {:type :word, :value "the", :rule 12, :index 1}
   {:type :word, :value "button", :rule 12, :index 2}
   {:type :word, :value "is", :rule 12, :index 3}
   {:type :word, :value "pressed", :rule 12, :index 4}
   {:type :word, :value ",", :rule 12, :index 5}
   {:type :word, :value "win", :rule 12, :index 6}
   {:type :rule}
   {:type :rule})
  )

(defmacro defobject [name & {:as body}]
  `(def ~name
     (map->GameObject
      ~(merge
        body
        {:name `'~name}))))

(defobject Natural
  ;; you complain about French numbers but then you have this abomination...
  :atn
  '{:ZERO {:s {:trans {"zero" [:pop 0]}}}
    :DIGIT {:s {:trans {"one" [:pop 1]
                        "two" [:pop 2]
                        "three" [:pop 3]
                        "four" [:pop 4]
                        "five" [:pop 5]
                        "six" [:pop 6]
                        "seven" [:pop 7]
                        "eight" [:pop 8]
                        "nine" [:pop 9]}}}
    :TEEN {:s {:trans {"ten" [:pop 10]
                       "eleven" [:pop 11]
                       "twelve" [:pop 12]
                       "thirteen" [:pop 13]
                       "fourteen" [:pop 14]
                       "fifteen" [:pop 15]
                       "sixteen" [:pop 16]
                       "seventeen" [:pop 17]
                       "eighteen" [:pop 18]
                       "nineteen" [:pop 19]}}}
    :TEN {:s {:trans {"twenty" [:pop 20]
                      "thirty" [:pop 30]
                      "forty" [:pop 40]
                      "fifty" [:pop 50]
                      "sixty" [:pop 60]
                      "seventy" [:pop 70]
                      "eighty" [:pop 80]
                      "ninety" [:pop 90]}}}
    :ONE-TO-99 {:s {:cats [[:DIGIT :e (assoc reg :n it)]
                           [:TEEN :e (assoc reg :n it)]
                           [:TEN :gt-ten (assoc reg :n it)]]}
                :gt-ten {:pop (:n reg)
                         :cats [[:DIGIT :e (update reg :n + it)]]}
                :e {:pop (:n reg)}}
    :ONE-TO-999 {:s {:cats [[:DIGIT :digit (assoc reg :n it)]
                            [:ONE-TO-99 :e (assoc reg :n it)]]}
                 :digit {:trans {"hundred" [:hundred reg]}}
                 :hundred {:trans {"and" [:hundred0 reg]}}
                 :hundred0 {:cats [[:ONE-TO-99
                                    :e
                                    (assoc reg :n
                                           (+ (* (:n reg) 100)
                                              it))]]}
                 :e {:pop (:n reg)}}
    :NATURAL {:s {:cats [[:ZERO :e (assoc reg :n it)]
                         [:ONE-TO-999 :e (assoc reg :n it)]]}
              :e {:pop (:n reg)}}})

(comment
  (semantic-construct.parser.atn/parse-and-suggest
   (merge {:s (sentence/sentence->atn-layer '[[:let n :NATURAL]] '(:n reg))}
          (:atn Natural))
   ["four" "hundred" "and" "twenty" "five"]))

(defobject Button
  :defs
  {'THING-THAT-EXISTS [merge {"button"
                              {:default-props
                               {:type :button}}}]})

(defobject TheGame
  :atn
  {:s '{:s {:cats [[:RULES :e (assoc reg :val {:type :rule, :rule it})]]}
        :e {:pop (:val reg)}}
   :RULES '{:s {:cats [[:THERE-IS :e (assoc reg :r it)]
                       [:WHEN :e (assoc reg :r it)]]}
            :e {:pop (:r reg)}}

   :THING-THAT-EXISTS '{:s {:dyn {:trans
                                  (into {}
                                        (map (fn [[word info]]
                                               [word [:pop (list 'quote info)]]))
                                        THING-THAT-EXISTS)}}}
   :THERE-IS '{:s {:trans {"there" [:s1 reg]}}
               :s1 {:trans {"is" [:s2 reg]}}
               :s2 {:trans {"a" [:s3 reg]}}
               :s3 {:cats [[:THING-THAT-EXISTS :e (assoc reg :thing (:default-props it))]]}
               :e {:pop {:type :add
                         :thing (:thing reg)}}}
   :WHEN
   '{:s {:trans {"when" [:s1 reg]}}
     :s1 {:cats [[:THING-THAT-CAN-HAPPEN :s2 (assoc reg :event it)]]}
     :s2 {:trans {"," [:s3 reg]}}
     :s3 {:cats [[:ACTION-THAT-CAN-BE-DONE :e (assoc reg :action it)]]}
     :e {:pop {:type :when
               :event (:event reg)
               :action (:action reg)}}}

   :DEFINITE-REFERENCE
   '{:s {:trans {"the" [:e reg]}}
     :e {:dyn {:trans
               (into
                {}
                (comp
                 (map (comp
                       (juxt name ;; type property name
                             (comp
                              ;; set of objects with [:type type]
                              (partial get ((comp :prop-pair-to-ids :properties) GAME))
                              ;; [:type type]
                              (partial vector :type)))
                       ;; object's type property
                       :type
                       ;; map of object's properties
                       (partial get (:id-to-props (:properties GAME)))
                       ;; id of object
                       ))
                 ;; filter those with non-singleton type pair
                 (filter (comp (partial = 1) count second))
                 (map (juxt first
                            (comp (partial vector :pop)
                                  ;; id
                                  first second))))
                ((comp :type :prop-to-ids :properties) GAME))}}}
   :REFERENCE
   '{:s {:cats [[:DEFINITE-REFERENCE :e (assoc reg :r it)]]}
     :e {:pop (:r reg)}}

   :ACTION
   '{:s {:trans {"is" [:is reg]}}
     :is {:trans {"clicked" [:pop {:type :click
                                   :receiver (:refd reg)}]}}}

   :THING-THAT-CAN-HAPPEN
   '{:s {:cats [[:REFERENCE :refd (assoc reg :refd it)]]}
     :refd {:cats [[:ACTION :e (assoc reg :event it)]]}
     :e {:pop (:event reg)}}

   :ACTION-THAT-CAN-BE-DONE
   '{:s {:trans {"win" [:pop {:type :win}]}}}})

(comment
  (let [[atn vars] (atn-and-vars TheGame Button)]
    (binding [ev/*mapped-syms*
              (merge ev/*mapped-syms*
                     vars
                     {'GAME (new-game {:type :button})})]
      (semantic-construct.parser.atn/parse-and-suggest
       atn
       ;;["when" "the" "button" "is" "clicked" "," "win"]
       ["there" "is" "a" "button"])))
  )

(defn merge-vars [vars defs]
  (reduce (fn [vars [key [merge-fn value]]]
            (if-let [old-val (get vars key)]
              (assoc! vars key (merge-fn old-val value))
              (assoc! vars key value)))
          vars
          defs))

(defn atn-and-vars [& objects]
  [(apply merge (map :atn objects))
   (persistent! (reduce merge-vars (transient {}) (map :defs objects)))])

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
