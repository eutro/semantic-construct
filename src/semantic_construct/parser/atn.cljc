(ns semantic-construct.parser.atn
  (:require [semantic-construct.parser.evaluator :as ev]))

(defn- check-guard [guard reg]
  (or (nil? guard)
      (ev/eval-action guard {'reg reg})))

(declare ^:private popped)
(defn- epsilons [atn state]
  (let [{[{:keys [net node cont reg], :as frame} & tail] :stack, :as state} state
        node (get-in atn [net node])]
    (apply
     concat
     (when (:trans node) [state])
     (when-let [popa (get node :pop nil)]
       (popped atn state (ev/eval-action popa {'reg reg})))
     (when-let [es (get node :epsilons nil)]
       (apply
        concat
        (for [[et ea guard] es]
          (when (check-guard guard reg)
            (epsilons
             atn
             {:stack
              (cons (assoc frame
                           :node et
                           :reg (ev/eval-action ea {'reg reg}))
                    tail)})))))
     (for [[cn ct ca guard] (get node :cats nil)]
       (when (check-guard guard reg)
         (epsilons
          atn
          {:stack
           (list*
            {:net cn
             :node :s
             :reg reg
             :cont ca}
            (assoc frame :node ct)
            tail)}))))))

(defn- popped [atn state res]
  (let [{[{:keys [cont]} & tail] :stack} state]
    (if cont
      (epsilons
       atn
       {:stack
        (cons
         (-> (first tail)
             (update :reg #(ev/eval-action cont {'reg %, 'it res})))
         (next tail))})
      [{:result res}])))

(defn- consume [atn state sym]
  (let [{[{:keys [net node reg], :as frame} & tail] :stack} state
        trans (get-in atn [net node :trans])]
    (when-let [got (get trans sym)]
      (if (set? trans)
        (popped atn state sym)
        (let [[tt ta guard] got]
          (when (check-guard guard reg)
            (if (= :pop tt)
              (popped atn state (ev/eval-action ta {'reg reg, 'it sym}))
              (epsilons
               atn
               {:stack
                (cons
                 (-> frame
                     (assoc :node tt)
                     (update :reg #(ev/eval-action ta {'reg %, 'it sym})))
                 tail)}))))))))

(defn start [atn]
  (epsilons atn {:stack [{:net :s, :node :s, :reg {}}]}))

(defn advance [atn states sym]
  (->> states
       (filter :stack)
       (mapcat #(consume atn % sym))))

(defn pparse [atn syms]
  (reduce (partial advance atn) (start atn) syms))

(defn finish-parse [states]
  (->> states
       (filter #(contains? % :result))
       (map :result)))

(defn parse [atn syms]
  (finish-parse (pparse atn syms)))

(defn suggest [atn states]
  (->> states
       (filter :stack)
       (mapcat (fn [{[{:keys [net node reg]}] :stack}]
                 (let [trans (get-in atn [net node :trans])]
                   (if (set? trans)
                     (seq trans)
                     (into nil
                           (comp
                            (filter (fn [[_ [tt ta guard]]]
                                      (check-guard guard reg)))
                            (map first))
                           trans)))))
       dedupe))

(defn psuggest [atn syms]
  (suggest atn (pparse atn syms)))

(defn parse-and-suggest [atn syms]
  (let [pp (pparse atn syms)]
    {:parses (finish-parse pp)
     :suggestions (suggest atn pp)}))
