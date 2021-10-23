(ns semantic-construct.game.feature
  #?(:cljs (:require-macros [semantic-construct.game.feature :refer [deffeature]]))
  (:require [semantic-construct.parser.sentence :as stc]
            [clojure.set :as set]
            [semantic-construct.parser.evaluator :as ev]))

(defrecord Feature [name atn defs])

(defmacro deffeature [name & {:as body}]
  `(def ~name
     (map->Feature
      ~(merge
        body
        {:name `'~name}))))

(deffeature Natural
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
   (merge {:s (stc/sentence->atn-layer '[[:let n :NATURAL]] '(:n reg))}
          (:atn Natural))
   ["four" "hundred" "and" "twenty" "five"]))

(deffeature Button
  :defs
  {'THING-THAT-EXISTS [set/union
                       #{{:word (fn [count] (if (= 1 count) "button" "buttons"))
                          :default-props {:type :button}}}]})

(deffeature Win
  :defs
  ;; "there is a win" guys :)))
  {'THING-THAT-EXISTS [set/union
                       #{{:word (fn [count] (if (= 1 count) "win" "wins"))
                          :default-props {:type :victory, :had false}}
                         {:word (fn [count] (if (= 1 count) "victory" "victories"))
                          :default-props {:type :victory, :had false}}}]})

(deffeature TheGame
  :defs
  {'THING-THAT-EXISTS [set/union #{}]}
  :atn
  {:s '{:s {:cats [[:RULES :e (assoc reg :val it)]]}
        :e {:pop (:val reg)}}
   :RULES '{:s {:cats [[:THERE-IS :e (assoc reg :r it)]
                       [:WHEN :e (assoc reg :r it)]]}
            :e {:pop (:r reg)}}

   :QUOTED '{:s {:trans {"\"" [:q1 (assoc reg :qm "\"")]
                         "'" [:q1 (assoc reg :qm "'")]}}
             :q1 {:dyn
                  {:trans
                   (dissoc
                    (into {}
                          (map (fn [id]
                                 (let [value
                                       (get-in @GAME
                                               [:properties :id-to-props id :value])]
                                   [value [:q2 (list 'assoc 'reg :v value)]])))
                          (get-in @GAME [:properties :prop-pair-to-ids [:type :word]]))
                    (:qm reg))}}
             :q2 {:dyn
                  {:trans {(:qm reg) '[:pop {:default-props {:type :word, :value (:v reg)}}]}}}}

   :THING-THAT-EXISTS '{:s {:cats [[:QUOTED :e (assoc reg :q it)]]
                            :dyn {:trans
                                  (into {}
                                        (map (fn [{:keys [word] :as info}]
                                               [(word (:count reg))
                                                [:pop (list 'quote info)]]))
                                        THING-THAT-EXISTS)}}
                        :e {:pop (:q reg)}}
   :THERE-IS '{:s {:trans {"there" [:s1 reg]}}
               :s1 {:trans {"is" [:s2 (assoc reg :count 1)]
                            "are" [:a1 reg]}}
               :a1 {:cats [[:NATURAL :a2 (assoc reg :count it)]]}
               :a2 {:epsilons [[:s3 reg (not= 1 (:count reg))]]}
               :s2 {:trans {"a" [:s3 reg]
                            "an" [:s3 reg]
                            "one" [:s3 reg]}}
               :s3 {:cats [[:THING-THAT-EXISTS :e (assoc reg :thing (:default-props it))]]}
               :e {:pop {:type :repeat
                         :count (:count reg)
                         :rule {:type :add
                                :thing (:thing reg)}}}}
   :WHEN
   '{:s {:trans {"when" [:s1 reg]}
         :cats [[:ACTION-THAT-CAN-BE-DONE :p1 (assoc reg :action it)]]}
     :s1 {:cats [[:THING-THAT-CAN-HAPPEN :s2 (assoc reg :event it)]]}
     :s2 {:trans {"," [:s3 reg]}}
     :s3 {:cats [[:ACTION-THAT-CAN-BE-DONE :e (assoc reg :action it)]]}
     :p1 {:trans {"when" [:p2 reg]}}
     :p2 {:cats [[:THING-THAT-CAN-HAPPEN :e (assoc reg :event it)]]}
     :e {:pop {:type :when
               :event (:event reg)
               :action (:action reg)}}}

   :INDEFINITE-REFERENCE
   '{:s {:trans {"a" [:e reg]
                 "an" [:e reg]}}
     :q {:pop (fn [id]
                (let [props (get-in @GAME [:id-to-props id])]
                  (if (= (:type props) :word)
                    (= (:value props) (:q reg))
                    false)))}
     :e {:cats [[:QUOTED :q (assoc reg :q it)]]
         :dyn {:trans
               (into
                {}
                (comp
                 (map (comp
                       ;; object's type property
                       :type
                       ;; map of object's properties
                       (partial get (:id-to-props (:properties @GAME)))
                       ;; id of object
                       ))
                 (distinct)
                 (map (juxt name
                            (fn [type]
                              [:pop
                               (list
                                'quote
                                (fn [x]
                                  (= type (get-in @GAME [:properties :id-to-props x :type]))))]))))
                ((comp :type :prop-to-ids :properties) @GAME))}}}

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
                              (partial get ((comp :prop-pair-to-ids :properties) @GAME))
                              ;; [:type type]
                              (partial vector :type)))
                       ;; object's type property
                       :type
                       ;; map of object's properties
                       (partial get (:id-to-props (:properties @GAME)))
                       ;; id of object
                       ))
                 ;; filter those with non-singleton type pair
                 (filter (comp (partial = 1) count second))
                 (map (juxt first
                            (comp (partial vector :pop)
                                  ;; id
                                  (partial partial =) first second))))
                ((comp :type :prop-to-ids :properties) @GAME))}}}
   :REFERENCE
   '{:s {:cats [[:DEFINITE-REFERENCE :e (assoc reg :r it)]
                [:INDEFINITE-REFERENCE :e (assoc reg :r it)]]}
     :e {:pop (:r reg)}}

   :ACTION
   '{:s {:trans {"is" [:is reg]}}
     :is {:trans {"pressed" [:pop {:type :click
                                   :receiver (:refd reg)}]}}}

   :THING-THAT-CAN-HAPPEN
   '{:s {:cats [[:REFERENCE :refd (assoc reg :refd it)]]}
     :refd {:cats [[:ACTION :e (assoc reg :event it)]]}
     :e {:pop (:event reg)}}

   :ACTION-THAT-CAN-BE-DONE
   '{:s {:trans {"win" [:pop {:type :win}]}}}})
