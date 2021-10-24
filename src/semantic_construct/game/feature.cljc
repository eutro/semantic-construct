(ns semantic-construct.game.feature
  (:require [clojure.set :as set]
            [semantic-construct.parser.atn :as atn]))

(def ^:dynamic *vars* nil)

(defrecord Thing [words product ctor])

(defn prop-set-op
  ([op tp] (-> (get-in *vars* [:game :properties :prop-pair-to-ids tp])))
  ([op tp1 tp2]
   (let [pp->ids (get-in *vars* [:game :properties :prop-pair-to-ids])]
     (op (pp->ids tp1) (pp->ids tp2))))
  ([op tp1 tp2 tp3 & tpr]
   (let [pp->ids (get-in *vars* [:game :properties :prop-pair-to-ids])]
     (apply op (map pp->ids (list* tp1 tp2 tp3 tpr))))))

(defn prop-intersection [& tps]
  (apply prop-set-op set/intersection tps))

(defn prop-union [& tps]
  (apply prop-set-op set/union tps))

(defn has-prop?
  ([id prop value] (has-prop? id [prop value]))
  ([id tp] ((get-in *vars* [:game :properties :prop-pair-to-ids]) id)))

(defn prop-all? [id & tps]
  (every? (partial has-prop? id) tps))

(defn prop-any? [id & tps]
  (some (partial has-prop? id) tps))

(defn product-all [product]
  (apply set/union (map (partial apply prop-intersection) product)))

(defn product-check? [product id]
  (some (partial apply prop-all?) product))

(defn thing-by-types [singular plural types & {:as ctor-props}]
  (let [product (into #{} (map (fn [ty] (merge ctor-props {:type ty}))) types)]
    (map->Thing
     {:words (fn [count] (if (= 1 count) singular plural))
      :product product
      :ctor (when (= 1 (count product)) (first product))})))

(def things-that-exist
  [(thing-by-types #{"object"}
                   #{"objects"}
                   #{:square :circle :triangle :victory})
   (thing-by-types #{"square"}
                   #{"squares"}
                   #{:square})
   (thing-by-types #{"circle"}
                   #{"circles"}
                   #{:circle})
   (thing-by-types #{"triangle"}
                   #{"triangles"}
                   #{:triangle})
   ;; "there is a win" because everybody is going to try it
   (thing-by-types #{"win"}
                   #{"wins"}
                   #{:victory}
                   :had false)
   (thing-by-types #{"victory"}
                   #{"victories"}
                   #{:victory}
                   :had false)])

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
                           (comp
                            (map #(get-in *vars* [:game :properties :id-to-props % :value]))
                            (dedupe)
                            (map (fn [value] [value [:q2 (fn [reg it] (assoc reg :v value))]])))
                           (prop-intersection [:type :word]))
                     (:qm reg))})},
            :q2 {:dyn
                 (fn [reg]
                   {:trans
                    {(:qm reg)
                     [:pop (fn [reg it] (:v reg))]}})}},

   :QUOTED-THING
   {:s {:cats [[:QUOTED :e
                (fn [reg it]
                  (assoc
                   reg
                   :q
                   (map->Thing
                    {:words (constantly #{})
                     :product #{{:type :word, :value it}}
                     :ctor {:type :word, :value it}})))]]}
    :e {:pop :q}}

   :THING
   {:s {:cats [[:QUOTED-THING :e (fn [reg it] (assoc reg :q it))]]
        :dyn
        (fn [reg]
          {:trans
           (into {}
                 (mapcat
                  (fn [{:keys [words], :as info}]
                    (let [rhs [:pop (constantly info)]]
                      (map (fn [word] [word rhs])
                           (words (or (:count reg) (throw (ex-info "Unknown count for thing." {}))))))))
                 things-that-exist)})},
    :e {:pop :q}},

   :RULES {:s {:cats [[:THERE-IS :e (fn [reg it] (assoc reg :r it)) nil]
                      [:WHEN :e (fn [reg it] (assoc reg :r it)) nil]]},
           :e {:pop :r}},

   :REFERENCE {:s {:cats [[:DEFINITE-REFERENCE :e (fn [reg it] (assoc reg :r it)) nil]
                          [:INDEFINITE-REFERENCE :e (fn [reg it] (assoc reg :r it)) nil]]},
               :e {:pop :r}},

   :DEFINITE-REFERENCE
   {:s {:trans {"the" [:s1 (fn [reg it] (assoc reg :count 1))]}},
    :s1 {:cats [[:THING :e (fn [reg it] (assoc reg :thing it))]]}
    :e {:pop (fn [{{:keys [product]} :thing}]
               (fn [id]
                 (let [all (product-all product)]
                   (and (= 1 (count all)) (all id)))))}}

   :INDEFINITE-REFERENCE
   {:s {:trans {"a" [:s1 (fn [reg it] (assoc reg :count 1))],
                "an" [:s1 (fn [reg it] (assoc reg :count 1))]}
        :cats [[:QUOTED-THING :e (fn [reg it] (assoc reg :thing it))]]},
    :s1 {:cats [[:THING :e (fn [reg it] (assoc reg :thing it))]]}
    :e {:pop (fn [{{:keys [product]} :thing}]
               (fn [id] (product-check? product id)))}}

   :WHEN {:s
          {:trans {"when" [:s1 (fn [reg it] reg)]},
           :cats [[:ACTION
                   :p1
                   (fn [reg it] (assoc reg :action it))]]},
          :s1
          {:cats [[:CONDITION
                   :s2
                   (fn [reg it] (assoc reg :event it))]]},
          :s2 {:trans {"," [:s3 (fn [reg it] reg)]}},
          :s3 {:cats [[:ACTION
                       :e
                       (fn [reg it] (assoc reg :action it))]]},
          :p1 {:trans {"when" [:p2 (fn [reg it] reg)]}},
          :p2 {:cats [[:CONDITION
                       :e
                       (fn [reg it] (assoc reg :event it))]]},
          :e {:pop (fn [reg] {:type :when, :event (:event reg), :action (:action reg)})}},

   :UNIVERSAL-REFERENCE
   {:s {:trans {"all" [:s1 (fn [reg it] (assoc reg :count ##Inf))]}}
    :s1 {:cats [[:THING :e (fn [reg it] (assoc reg :thing it))]]}
    :e {:pop :thing}}

   :EXISTENTIAL-REFERENCE
   {:s {:trans {"any" [:s1 (fn [reg it] (assoc reg :count 1))]}}
    :s1 {:cats [[:THING :e (fn [reg it] (assoc reg :thing it))]]}
    :e {:pop :thing}}

   :BOOLEAN-EXPR
   {:s {:epsilons [[:s1 (fn [reg] (assoc reg :there-is/bool true))]]}
    :s1 {:cats [[:THERE-IS :s2 (fn [reg it] (assoc reg :cond it))]]}
    :s2 {:pop :cond}}

   :CONDITION
   {:s {:cats [[:REFERENCE :refd (fn [reg it] (assoc reg :refd it))]
               [:BOOLEAN-EXPR :bool (fn [reg it] (assoc reg :bool it))]]},
    :refd {:trans {"is" [:is (fn [reg it] reg)]}}
    :is {:trans {"pressed" [:pop (fn [reg it]
                                   {:type :click
                                    :receiver (:refd reg)})]}}
    :bool {:pop (fn [reg]
                  {:type :tick
                   :pred (:cond reg)})}}

   :ACTION
   {:s {:trans {"win" [:pop (constantly {:type :win})]}}},

   :PREFIX-CMP
   {:s {:trans {"less" [:than (fn [reg it] (assoc reg :cmp <))]
                "more" [:than (fn [reg it] (assoc reg :cmp >))]
                "at" [:at (fn [reg it] reg)]}}
    :at {:trans {"least" [:pop (fn [reg it] (assoc reg :cmp >=))]
                 "most" [:pop (fn [reg it] (assoc reg :cmp <=))]}}
    :than {:trans {"than" [:pop (fn [reg it] (:cmp reg))]}}}

   :THERE-IS
   {:s {:trans {"there" [:s1 (fn [reg it] reg)]}},
    :s1 {:trans {"is" [:s2 (fn [reg it] (assoc reg :count 1))],
                 "are" [:a1 (fn [reg it] reg)]}},
    :a1 {:cats [[:NATURAL :a2 (fn [reg it] (assoc reg :count it)) nil]]
         :dyn (fn [reg]
                (when (and (:there-is/bool reg) (not (:cmp reg)))
                  {:cats [[:PREFIX-CMP :a1 (fn [reg it] (assoc reg :cmp it))]]}))},
    :a2 {:epsilons [[:s3 identity (fn [reg] (not= 1 (:count reg)))]]},
    :s2 {:trans {"a" [:s3 (fn [reg it] (assoc reg :count 1))],
                 "an" [:s3 (fn [reg it] (assoc reg :count 1))],
                 "one" [:s3 (fn [reg it] (assoc reg :count 1))]}
         :dyn (fn [reg]
                (when (and (:there-is/bool reg) (not (:cmp reg)))
                  {:cats [[:PREFIX-CMP :s2 (fn [reg it] (assoc reg :cmp it))]]}))},
    :s3 {:cats [[:THING :e (fn [reg it] (assoc reg :thing it))]]},
    :e {:dyn (fn [reg]
               {:epsilons (if (:there-is/bool reg)
                            [[:b identity (fn [reg] (:cmp reg))]]
                            [[:e1 identity (fn [reg] (:ctor (:thing reg)))]])})}
    :b {:pop
        (fn [{{:keys [product]} :thing, n :count, :keys [cmp]}]
          (fn [] (cmp (count (product-all product)) n)))}
    :e1 {:pop
         (fn [reg]
           {:type :repeat,
            :count (:count reg),
            :rule {:type :add, :thing (:ctor (:thing reg))}})}}

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
                :hundred {:trans {"and" [:hundred1 (fn [reg it] reg)]}
                          :pop (fn [reg] (* 100 (:n reg)))},
                :hundred1 {:cats
                           [[:ONE-TO-99
                             :e
                             (fn [reg it] (assoc reg :n (+ (* (:n reg) 100) it)))
                             nil]]},
                :e {:pop :n}},

   :NATURAL {:s {:cats
                 [[:ZERO :e (fn [reg it] (assoc reg :n it)) nil]
                  [:ONE-TO-999 :e (fn [reg it] (assoc reg :n it)) nil]]},
             :e {:pop :n}}})

(comment
  (require '[semantic-construct.game.state :as s]
           '[semantic-construct.game.engine :as e])
  (-> (s/new-game)
      (s/add-init-rules ["win" "when" "\"" "pressed" "\"" "is" "pressed"])
      (e/on-change))
  (atn/parse-and-suggest atn ["win" "when" "there" "are" "at" "least" "zero" "triangles"])
  ;;
  )
