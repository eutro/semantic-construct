(ns semantic-construct.parser.evaluator
  (:refer-clojure :exclude [destructure])
  #?(:cljs (:require-macros [semantic-construct.parser.evaluator :refer [var-map]]))
  (:require [semantic-construct.parser.destructure :refer [destructure]]))

(defmacro var-map [& syms]
  (into {} (map (fn [sym] [`'~sym `#'~sym])) syms))

(def ^:dynamic *mapped-syms*
  (merge
   (var-map
    assoc select-keys update conj reverse vec first next
    cons = not= + * / - str zero? mod rem apply into map
    comp partial count filter second merge list frequencies
    get find vector vec name juxt nth seq seq? hash-map)
   {'dbg #(doto % prn)}))

(defn eval-action
  ([action] (eval-action action nil))
  ([action env]
   (((fn eval-with-env [env]
       (fn eval-form [form]
         (cond (seq? form)
               (if-let [[func & args] (seq form)]
                 (case func
                   quote (let [[quoted & tail] args]
                           (when tail
                             (throw (ex-info "too many arguments to quote" {:form form})))
                           quoted)

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
                                      (throw (ex-info "let bindings must all be symbols"
                                                      {:form form, :lhs lhs})))
                                    (rf env [lhs ((eval-with-env env) rhs)]))
                                   ([x] (rf x)))))
                              (destructure spec))]
                         ((eval-with-env env) body))

                   fn (let [[spec & body] args
                            _ (when-not (vector? spec)
                                (throw (ex-info "fn spec must be a vector"
                                                {:form form})))
                            [spec body]
                            (if (every? symbol? spec)
                              [spec body]
                              (let [params
                                    (repeatedly (count spec)
                                                (partial gensym "p"))]
                                [(vec params)
                                 (list
                                  (list* 'let
                                         (vec (interleave spec params))
                                         body))]))]
                        (fn [& args]
                          (when-not (= (count args) (count spec))
                            (throw (ex-info "arity mismatch"
                                            {:expected (count spec)
                                             :received (count args)
                                             :form form})))
                          (let [eval-form (eval-with-env (into env (map vector spec args)))]
                            (last (map eval-form body)))))

                   (apply (eval-form func)
                          (map eval-form args)))
                 ())

               (symbol? form)
               (if-let [v (get env form)]
                 v
                 (throw (ex-info "undefined variable"
                                 {:var form,
                                  :env (try (persistent! env)
                                            (catch #?(:cljs js/Object :clj Exception) e
                                              env))})))

               (map? form)
               (into {} (map (partial mapv eval-form)) form)

               (vector? form)
               (mapv eval-form form)

               (set? form)
               (into #{} (map eval-form form))

               :else form)))
     (merge *mapped-syms* env))
    action)))

(comment
  (eval-action '(let [[x y z] (vec '{x 1 y 2 z 3})] x))
  )
