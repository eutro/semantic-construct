(ns semantic-construct.game.objects
  "Contains all the objects that exist in a game."
  (:require #?(:cljs [semantic-construct.game.objects :refer-macros [defobject]])
            [semantic-construct.parser.sentence :as sentence]))

(defrecord Game [objects events rules])

(defrecord Rule [id words apply unapply])

(defrecord Property [type])

(defn propset [& properties]
  (into (sorted-map)
        (comp (partition-all 2)
              (map (fn [[k v]]
                     [k (->Property v)])))
        properties))

(defrecord GameObject [name properties atn])

(defmacro defobject [name & body]
  `(def ~name
     (->GameObject
      '~name
      ~@body)))

(defobject Natural
  (propset)
  ;; you complain about French numbers but then you have this abomination...
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
   ["four" "hundred" "and" "twenty" "two"]))

(defobject Board
  (propset "width" Natural
           "height" Natural))

(defn rule<=>
  "Comparator that sorts rules in descending order by ID and then by words."
  [r1 r2]
  (let [f (juxt :id :words)]
    (compare (f r2) (f r1))))

(defn new-game []
  (map->Game
   {:objects {}
    :events {}
    :rules (sorted-set-by rule<=>)}))

(defn conj-rule [game rule]
  (update
   game
   :rules
   (fn [rules]
     (let [id (if-let [{x :id} (first rules)] (inc' x) 0)]
       (conj rules (assoc rule :id id))))))
