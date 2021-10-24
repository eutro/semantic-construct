(ns semantic-construct.game.feature)

(def ^:dynamic *vars*)

(def things-that-exist
  [{:word (fn [count] (if (= 1 count) "button" "buttons"))
    :default-props {:type :button}}
   ;; "there is a win" :))
   {:word (fn [count] (if (= 1 count) "win" "wins"))
    :default-props {:type :victory, :had false}}
   {:word (fn [count] (if (= 1 count) "victory" "victories"))
    :default-props {:type :victory, :had false}}])

(def atn
  {:s {:s {:cats [[:RULES :e (fn [reg it] (assoc reg :val it)) nil]]},
       :e {:pop :val}}

   :QUOTED {:s {:trans {"\"" [:q1 (fn [reg it] (assoc reg :qm "\""))],
                        "'" [:q1 (fn [reg it] (assoc reg :qm "'"))]}},
            :q1 {:dyn
                 (fn [reg]
                   {:trans
                    (dissoc
                     (into {}
                           (map
                            (fn [id]
                              (let [value (get-in *vars* [:game :properties :id-to-props id :value])]
                                [value [:q2 (fn [reg it] (assoc reg :v value))]])))
                           (get-in *vars* [:game :properties :prop-pair-to-ids [:type :word]]))
                     (:qm reg))})},
            :q2 {:dyn
                 (fn [reg]
                   {:trans
                    {(:qm reg)
                     [:pop (fn [reg it] (:v reg))]}})}},

   :THING-THAT-EXISTS
   {:s {:cats [[:QUOTED :e
                (fn [reg it]
                  (assoc reg :q {:default-props {:type :word, :value it}}))]],
        :dyn
        (fn [reg]
          {:trans
           (into {}
                 (map
                  (fn [{:keys [word], :as info}]
                    [(word (:count reg)) [:pop (constantly info)]]))
                 things-that-exist)})},
    :e {:pop :q}},

   :RULES {:s {:cats [[:THERE-IS :e (fn [reg it] (assoc reg :r it)) nil]
                      [:WHEN :e (fn [reg it] (assoc reg :r it)) nil]]},
           :e {:pop :r}},

   :REFERENCE {:s {:cats [[:DEFINITE-REFERENCE :e (fn [reg it] (assoc reg :r it)) nil]
                          [:INDEFINITE-REFERENCE :e (fn [reg it] (assoc reg :r it)) nil]]},
               :e {:pop :r}},

   :DEFINITE-REFERENCE
   {:s {:trans {"the" [:e (fn [reg it] reg)]}},
    :e {:dyn
        (fn [reg]
          {:trans
           (into
            {}
            (comp (map (fn [id] (get-in *vars* [:game :properties :id-to-props id :type])))
                  (dedupe)
                  (map (fn [type]
                         (let [pred
                               (fn [id]
                                 (let [properties (-> *vars* :game :properties)]
                                   (and (-> properties
                                            :id-to-props (get id)
                                            :type (= type))
                                        (-> properties
                                            :prop-pair-to-ids (get [:type type])
                                            count (= 1)))))]
                           [(name type) [:pop (constantly pred)]]))))
            (-> *vars* :game :properties :prop-to-ids :type))})}},
   :WHEN {:s
          {:trans {"when" [:s1 (fn [reg it] reg)]},
           :cats [[:ACTION-THAT-CAN-BE-DONE
                   :p1
                   (fn [reg it] (assoc reg :action it))]]},
          :s1
          {:cats [[:THING-THAT-CAN-HAPPEN
                   :s2
                   (fn [reg it] (assoc reg :event it))]]},
          :s2 {:trans {"," [:s3 (fn [reg it] reg)]}},
          :s3 {:cats [[:ACTION-THAT-CAN-BE-DONE
                       :e
                       (fn [reg it] (assoc reg :action it))]]},
          :p1 {:trans {"when" [:p2 (fn [reg it] reg)]}},
          :p2 {:cats [[:THING-THAT-CAN-HAPPEN
                       :e
                       (fn [reg it] (assoc reg :event it))]]},
          :e {:pop (fn [reg] {:type :when, :event (:event reg), :action (:action reg)})}},
   :INDEFINITE-REFERENCE
   {:s {:trans {"a" [:e (fn [reg it] reg)],
                "an" [:e (fn [reg it] reg)]}
        :epsilons [[:pq identity]]},
    :s1 {:epsilons [[:e identity]
                    [:pq identity]]}
    :pq {:cats [[:QUOTED :q (fn [reg it] (assoc reg :q it))]]},
    :q {:pop (fn [reg]
               (fn [id]
                 (let [props (get-in *vars* [:game :properties :id-to-props id])]
                   (and (= (:type props) :word)
                        (= (:value props) (:q reg))))))},
    :e {:dyn
        (fn [reg]
          {:trans
           (into
            {}
            (comp (map (fn [id] (get-in *vars* [:game :properties :id-to-props id :type])))
                  (dedupe)
                  (map (fn [type]
                         (let [pred
                               (fn [id]
                                 (-> *vars* :game :properties
                                     :id-to-props (get id)
                                     :type (= type)))]
                           [(name type) [:pop (constantly pred)]]))))
            (-> *vars* :game :properties :prop-to-ids :type))})}},

   :ACTION
   {:s {:trans {"is" [:is (fn [reg it] reg)]}},
    :is {:trans {"pressed" [:pop (fn [reg it] {:type :click, :receiver (:refd reg)})]}}},

   :THING-THAT-CAN-HAPPEN
   {:s {:cats [[:REFERENCE :refd (fn [reg it] (assoc reg :refd it)) nil]]},
    :refd {:cats [[:ACTION :e (fn [reg it] (assoc reg :event it)) nil]]},
    :e {:pop :event}},

   :ACTION-THAT-CAN-BE-DONE {:s {:trans {"win" [:pop (constantly {:type :win})]}}},

   :THERE-IS {:s {:trans {"there" [:s1 (fn [reg it] reg)]}},
              :s1 {:trans {"is" [:s2 (fn [reg it] (assoc reg :count 1))],
                           "are" [:a1 (fn [reg it] reg)]}},
              :a1 {:cats [[:NATURAL :a2 (fn [reg it] (assoc reg :count it)) nil]]},
              :a2 {:epsilons [[:s3 identity (fn [reg] (not= 1 (:count reg)))]]},
              :s2 {:trans {"a" [:s3 (fn [reg it] reg)],
                           "an" [:s3 (fn [reg it] reg)],
                           "one" [:s3 (fn [reg it] reg)]}},
              :s3 {:cats [[:THING-THAT-EXISTS :e
                           (fn [reg it] (assoc reg :thing (:default-props it)))]]},
              :e {:pop
                  (fn [reg]
                    {:type :repeat,
                     :count (:count reg),
                     :rule {:type :add, :thing (:thing reg)}})}}
   ;; you complain about French numbers but then you have this abomination...
   :ZERO {:s {:trans {"zero" [:pop (constantly 0)]}}},
   :DIGIT {:s {:trans {"six" [:pop (constantly 6)],
                       "three" [:pop (constantly 3)],
                       "two" [:pop (constantly 2)],
                       "seven" [:pop (constantly 7)],
                       "five" [:pop (constantly 5)],
                       "eight" [:pop (constantly 8)],
                       "one" [:pop (constantly 1)],
                       "nine" [:pop (constantly 9)],
                       "four" [:pop (constantly 4)]}}},
   :TEEN {:s {:trans {"eighteen" [:pop (constantly 18)],
                      "twelve" [:pop (constantly 12)],
                      "fifteen" [:pop (constantly 15)],
                      "fourteen" [:pop (constantly 14)],
                      "eleven" [:pop (constantly 11)],
                      "nineteen" [:pop (constantly 19)],
                      "seventeen" [:pop (constantly 17)],
                      "thirteen" [:pop (constantly 13)],
                      "ten" [:pop (constantly 10)],
                      "sixteen" [:pop (constantly 16)]}}},
   :TEN {:s {:trans {"twenty" [:pop (constantly 20)],
                     "thirty" [:pop (constantly 30)],
                     "forty" [:pop (constantly 40)],
                     "fifty" [:pop (constantly 50)],
                     "sixty" [:pop (constantly 60)],
                     "seventy" [:pop (constantly 70)],
                     "eighty" [:pop (constantly 80)],
                     "ninety" [:pop (constantly 90)]}}},
   :ONE-TO-99 {:s {:cats [[:DIGIT :e (fn [reg it] (assoc reg :n it)) nil]
                          [:TEEN :e (fn [reg it] (assoc reg :n it)) nil]
                          [:TEN :gt-ten (fn [reg it] (assoc reg :n it)) nil]]},
               :gt-ten {:pop :n,
                        :cats [[:DIGIT :e (fn [reg it] (update reg :n + it)) nil]]},
               :e {:pop :n}},
   :ONE-TO-999 {:s {:cats [[:DIGIT :digit (fn [reg it] (assoc reg :n it)) nil]
                           [:ONE-TO-99 :e (fn [reg it] (assoc reg :n it)) nil]]},
                :digit {:trans {"hundred" [:hundred (fn [reg it] reg)]}},
                :hundred {:trans {"and" [:hundred0 (fn [reg it] reg)]}},
                :hundred0 {:cats
                           [[:ONE-TO-99
                             :e
                             (fn [reg it] (assoc reg :n (+ (* (:n reg) 100) it)))
                             nil]]},
                :e {:pop :n}},
   :NATURAL {:s {:cats
                 [[:ZERO :e (fn [reg it] (assoc reg :n it)) nil]
                  [:ONE-TO-999 :e (fn [reg it] (assoc reg :n it)) nil]]},
             :e {:pop :n}}})
