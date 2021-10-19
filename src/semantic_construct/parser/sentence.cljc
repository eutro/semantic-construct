(ns semantic-construct.parser.sentence
  "Utilities for constructing ATNs from simple sentences."
  (:require [clojure.core.match
             #?(:cljs :refer-macros
                :clj :refer)
             [match]]))

(defn word->step
  ([next-step word] (word->step next-step word 'reg nil))
  ([next-step word action guard]
   (match word
     (literal :guard string?)
     {:trans {literal [next-step action guard]}}

     (subnet :guard keyword?)
     {:cats [[subnet next-step action guard]]}

     [:let (sym :guard symbol?) new-word]
     (word->step next-step
                 new-word
                 (list 'assoc action (keyword sym) 'it)
                 guard)

     [:guard new-guard new-word]
     (word->step next-step
                 new-word
                 action
                 (if (nil? guard)
                   new-guard
                   (list 'and guard new-guard))))))

(defn sentence->atn-layer [sentence result]
  (let [steps
        (concat [:s]
                (map #(keyword (str "s" %))
                     (range 1 (count sentence)))
                [:e])]
    (as-> sentence $
      (map word->step (rest steps) $)
      (map vector steps $)
      (into {:e {:pop result}} $))))

(comment
  (require '[semantic-construct.parser.atn :as atn]
           '[semantic-construct.parser.evaluator :as ev])
  (def test-atn
    {:s '{:s {:cats [[:dog :e it]]}
          :e {:pop reg}}
     :colour {:s {:trans #{"brown" "green" "red"}}}
     :adj {:s {:trans {"lazy" [:pop "lazy"]
                       "lively" [:pop "lazey"]}}}
     :dog
     (sentence->atn-layer
      '["the" "quick"
        [:let fox-colour :colour]
        "fox" "jumps" "over" "the"
        [:let dog-adj :adj]
        [:guard (= "lazy" (:dog-adj reg)) "dog"]]
      '(->FoxJumping (:fox-colour reg) (:dog-adj reg)))})
  (binding [ev/*mapped-syms* (merge ev/*mapped-syms*
                                    {'->FoxJumping vector})]
    (atn/parse test-atn ["the" "quick" "brown" "fox" "jumps" "over" "the" "lazy" "dog"])))
