(ns semantic-construct.parser.parser
  (:require [semantic-construct.parser.atn :as atn]
            [semantic-construct.parser.evaluator :as ev]
            [semantic-construct.game.objects :as o]))

(def natural-atn
  '{:NATURAL-ONE-TO-NINE
    {:s {:trans {"one" [:e (assoc reg :n 1)]
                 "two" [:e (assoc reg :n 2)]
                 "three" [:e (assoc reg :n 3)]
                 "four" [:e (assoc reg :n 4)]
                 "five" [:e (assoc reg :n 5)]
                 "six" [:e (assoc reg :n 6)]
                 "seven" [:e (assoc reg :n 7)]
                 "eight" [:e (assoc reg :n 8)]
                 "nine" [:e (assoc reg :n 9)]}}
     :e {:pop (:n reg)}}})

(def dims-atn
  (merge
   natural-atn
   '{:DIMS-W-by-H
     {:s {:cats [[:NATURAL-ONE-TO-NINE :s1 (assoc reg :first it)]]}
      :s1 {:trans {"wide" [:wide {:wide (:first reg)}]
                   "high" [:high {:high (:first reg)}]
                   "by" [:w-by-h {:first (:first reg)}]}}

      :w-by-h {:cats [[:NATURAL-ONE-TO-NINE :first-second
                       (assoc reg :second it)]]}
      :first-second {:pop {:w (:first it)
                           :h (:second it)}}

      :wide {:trans {"and" [:wide1 reg]
                     "by" [:wide1 reg]}}
      :wide1 {:cats [[:NATURAL-ONE-TO-NINE :wide2 (assoc reg :high it)]]}
      :wide2 {:trans {"high" [:wide-high reg]}}

      :high {:trans {"and" [:high1 reg]
                     "by" [:high1 reg]}}
      :high1 {:cats [[:NATURAL-ONE-TO-NINE :high2 (assoc reg :wide it)]]}
      :high2 {:trans {"wide" [:wide-high reg]}}

      :wide-high {:pop {:w (:wide reg)
                        :h (:high reg)}}}}))

(def test-atn
  (merge
   dims-atn
   '{:s {:s {:cats [[:GAME-IS it]]}}

     :GAME-ON-A {:s {:cats [[:DIMS-W-by-H :dims {:dims it}]]}
                 :dims {:trans {"board" [:e (assoc reg :ty :grid)]
                                "grid" [:e (assoc reg :ty :grid)]}}
                 :e {:pop reg}}
     :GAME-IS {:s {:trans {"the" [:s1 reg]}}
               :s1 {:trans {"game" [:s2 reg]}}
               :s2 {:trans {"is" [:s3 reg]}}
               :s3 {:trans {"played" [:s4 reg]}}
               :s4 {:trans {"on" [:s5 reg]}}
               :s5 {:trans {"a" [:s6 reg]}}
               :s6 {:cats [[:GAME-ON-A :e {:keys [:on]
                                           :value it}]]}
               :e {:pop {:ty :assoc-in
                         :keys (cons :game (:keys reg))
                         :value (:value reg)}}}}))

#_
(binding [ev/*mapped-syms*
          (merge ev/*mapped-syms*
                 {'map->Grid o/map->Grid})]
  (atn/parse
   test-atn
   ["the"
    "game"
    "is"
    "played"
    "on"
    "a"
    "two"
    "high"
    "and"
    "six"
    "wide"
    "grid"]))
