(ns semantic-construct.render)

(defmacro with-ctx-bindings [bindings & body]
  (let [pbs (partition 2 bindings)
        propnames (map first pbs)
        propvals (map second pbs)
        refs (map (fn [propname] `(. ctx ~(symbol (str "-" (name propname)))))
                  propnames)
        set-all (fn [values]
                  (map (fn [ref value]
                         `(set! ~ref ~value))
                       refs
                       values))]
    `(do (.save ctx)
         (try
           ~@(set-all propvals)
           ~@body
           (finally (.restore ctx))))))

(defmacro obj-with-bindings [obj & bindings]
  `(update ~obj :thunk
           (fn [thunk#]
             (fn []
               (update (with-ctx-bindings [~@bindings] (thunk#)) :plot
                       (fn [plot#]
                         (fn [this#]
                           (with-ctx-bindings [~@bindings]
                             (plot# this#)))))))))
