(ns semantic-construct.parser.evaluator)

(def ^:dynamic *mapped-syms*
  (select-keys
   (ns-interns 'clojure.core)
   '[assoc select-keys update conj reverse vec first next
     cons = not= + * / -]))

(defn eval-action
  ([action] (eval-action action nil))
  ([action env]
   (((fn eval-with-env [env]
       (fn eval-form [form]
         (cond (list? form)
               (if-let [[func & args] (seq form)]
                 (case func
                   if (let [[pred then else & tail] args]
                        (when tail
                          (throw (ex-info "too many arguments to if form" {:form form})))
                        (eval-form (if (eval-form pred) then else)))

                   let (let [[spec body & tail] args
                             _ (do (when-not (vector? spec)
                                     (throw (ex-info "let spec must be a vector" {:form form})))
                                   (when-not (even? (count spec))
                                     (throw (ex-info "let spec must be an even number of forms" {:form form})))
                                   (when tail
                                     (throw (ex-info "too many forms to let form" {:form form}))))
                             env
                             (into
                              env
                              (comp
                               (partition-all 2)
                               (fn [rf]
                                 (fn
                                   ([] (rf)) ;; unused
                                   ([env [lhs rhs]]
                                    (when-not (symbol? lhs)
                                      (throw (ex-info "let bindings must all be symbols" {:form form, :lhs lhs})))
                                    (rf env [lhs ((eval-with-env env) rhs)]))
                                   ([x] (rf x)))))
                              spec)]
                         ((eval-with-env env) body))

                   (apply (eval-form func)
                          (map eval-form args)))
                 ())

               (symbol? form)
               (if-let [[_ v] (find env form)]
                 v
                 (throw (ex-info "undefined variable" {:var form, :env env})))

               (map? form)
               (into {} (map (partial mapv eval-form)) form)

               (vector? form)
               (mapv eval-form form)

               (set? form)
               (into #{} (map eval-form form))

               :else form)))
     (merge *mapped-syms* env))
    action)))
