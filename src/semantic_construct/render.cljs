(ns semantic-construct.render
  (:require-macros [semantic-construct.render :refer [with-ctx-bindings]])
  (:require [semantic-construct.mouse :as mouse]))

(def canvas (. js/document (getElementById "canvas")))
(def ctx (.getContext canvas "2d"))

(defrecord Matrix [a b c
                   d e f])
(defn dp3 [a1 b1 c1
           a2 b2 c2]
  (+ (* a1 a2)
     (* b1 b2)
     (* c1 c2)))
(defn mulm [{a1 :a,  b1 :b, c1 :c
             d1 :d,  e1 :e, f1 :f}
            {a2 :a,  b2 :b, c2 :c
             d2 :d,  e2 :e, f2 :f}]
  (->Matrix
   (dp3 a1 b1 c1, a2 d2 0)
   (dp3 a1 b1 c1, b2 e2 0)
   (dp3 a1 b1 c1, c2 f2 1)

   (dp3 d1 e1 f1, a2 d2 0)
   (dp3 d1 e1 f1, b2 e2 0)
   (dp3 d1 e1 f1, c2 f2 1)))
(defn mulv [{:keys [a b c
                    d e f]
             x y}]
  [(dp3 x y 1, a b c)
   (dp3 x y 1, d e f)])

(defprotocol Transform
  (transform [this matrix]))

(def identity-matrix (translation 0 0))

(defn translation [dx dy]
  (->Matrix
   1 0 dx
   0 1 dy))

(defn translate [this dx dy]
  (transform this (translation dx dy)))

(defn scale
  ([this sf] (transform this (scale sf)))
  ([sf]
   (->Matrix
    sf 0 0
    0 sf 0)))

(defrecord BB [minx miny maxx maxy]
  Transform
  (transform [this mat]
    (let [[minx miny] (mulv mat minx miny)
          [maxx maxy] (mulv mat maxx maxy)]
      (BB. minx miny maxx maxy))))
(defrecord InCtxRenderObject [bb mat plot]
  Transform
  (transform [this omat]
    (-> this
        (assoc :mat (mulm omat mat))
        (update :bb transform omat))))
(defrecord RenderObject [thunk]
  Transform
  (transform [this mat] (assoc this :thunk #(transform (thunk) mat)))
  IDeref
  (-deref [this] (thunk)))
(defn object [thunk]
  (->RenderObject thunk))

(defrecord Scene [objects])

(defn bb-contains? [{:keys [minx miny maxx maxy]} x y]
  (and (<= minx x maxx)
       (<= miny y maxy)))

(defn lines-overlap? [amin amax bmin bmax]
  (if (<= amin bmin)
    (<= bmin amax)
    (<= amin bmax)))

(defn bb-intersects? [bba bbb]
  (and (lines-overlap? (:minx bba) (:maxx bba) (:minx bbb) (:maxx bbb))
       (lines-overlap? (:miny bba) (:maxy bba) (:miny bbb) (:maxy bbb))))

(comment
  (bb-intersects? (->BB 0 0 1 1)
                  (->BB -1 -1 1 -1))
  ;
  )

(defn under-mouse [scene]
  (let [[mx my] @mouse/mouse]
    (filter #(bb-contains? (:bb @%) mx my) (:objects scene))))

(defn enlarge [{:keys [minx miny maxx maxy]} by]
  (->BB (- minx by)
        (- miny by)
        (+ maxx by)
        (+ maxy by)))

(defn plot-object [object]
  ((:plot object) object))

(defn plot-scene [scene]
  (.save ctx)
  (try
    (run! plot-object (map deref (:objects scene)))
    (finally
      (.restore ctx)))
  nil)

(def sprites (js/document.getElementById "sprites"))
(defn sprite-object
  ([sprites] (sprite-object sprites 0 0 (.-width sprites) (.-height sprites)))
  ([sprites sx sy sw sh]
   (sprite-object sprites (constantly (->BB 0 0 sw sh)) sx sy sw sh))
  ([sprites bb-thunk sx sy sw sh]
   (object
    (fn []
      (->InCtxRenderObject
       (bb-thunk)
       identity-matrix
       (fn [{{:keys [minx miny maxx maxy]} :bb}]
         (.drawImage ctx
                     sprites
                     sx sy
                     sw sh
                     minx miny
                     (- maxx minx)
                     (- maxy miny))))))))

(defn rect [bb-thunk]
  (object
   (fn []
     (->InCtxRenderObject
      (bb-thunk)
      identity-matrix
      (fn [{{:keys [minx miny maxx maxy]} :bb}]
        ;; the matrix is already captured in the bb
        (.fillRect ctx minx miny (- maxx minx) (- maxy miny)))))))

(defn ellipse [bb-thunk]
  (object
   (fn []
     (->InCtxRenderObject
      (bb-thunk)
      identity-matrix
      (fn [{{:keys [minx miny maxx maxy]} :bb}]
        (doto ctx
          .beginPath
          (.ellipse
           (/ (+ maxx minx) 2)
           (/ (+ maxy miny) 2)
           (/ (- maxx minx) 2)
           (/ (- maxy miny) 2)
           #_:rotation 0
           #_:start 0
           #_:end (* 2 Math/PI))
          .fill))))))

(defn triangle [bb-thunk]
  (object
   (fn []
     (->InCtxRenderObject
      (bb-thunk)
      identity-matrix
      (fn [{{:keys [minx miny maxx maxy]} :bb}]
        (doto ctx
          .beginPath
          (.moveTo minx maxy)
          (.lineTo (/ (+ maxx minx) 2) miny)
          (.lineTo maxx maxy)
          .fill))))))

(defn stroke-rect [bb-thunk]
  (object
   (fn []
    (->InCtxRenderObject
     (bb-thunk)
     identity-matrix
     (fn [{{:keys [minx miny maxx maxy]} :bb}]
        (.strokeRect ctx minx miny (- maxx minx) (- maxy miny)))))))

(defn full-bb []
  (let [cvs (.-canvas ctx)]
    (->BB 0 0 (.-width cvs) (.-height cvs))))

(def fill (rect full-bb))

(defn text [text]
  (object
   (fn []
     (let [text-metrics (.measureText ctx text)
           bb
           (->BB
            (- (Math/abs (.-actualBoundingBoxLeft text-metrics)))
            (- (Math/abs (.-actualBoundingBoxAscent text-metrics)))
            (Math/abs (.-actualBoundingBoxRight text-metrics))
            (Math/abs (.-actualBoundingBoxDescent text-metrics)))]
        (->InCtxRenderObject
         bb
         identity-matrix
         (fn [{:keys [mat]}]
           (.save ctx)
           (.setTransform ctx mat)
           (.fillText ctx text 0 0)
           (.restore ctx)))))))

(defn merge-bbs [& bbs]
  (->BB
   (transduce (map :minx) min bbs)
   (transduce (map :miny) min bbs)
   (transduce (map :maxx) max bbs)
   (transduce (map :maxy) max bbs)))

(defn combine [& objs]
  (object
   (fn []
     (let [derefd (map deref objs)]
       (->InCtxRenderObject
        (apply merge-bbs (map :bb derefd))
        identity-matrix
        (fn [{:keys [mat]}]
          (run! plot-object (map #(transform % mat) derefd))))))))

(defn center [obj & axes]
  (let [axis? (set axes)]
    (update
     obj
     :thunk
     (fn [thunk]
       (fn []
         (let [cw (.-width canvas), ch (.-height canvas)
               {{:keys [minx miny maxx maxy]} :bb, :as drfd} (thunk)]
           (translate
            drfd
            (if (axis? :x) (/ (- cw (- maxx minx)) 2) 0)
            (if (axis? :y) (/ (- ch (- maxy miny)) 2) 0))))))))

(defn truncate-inbounds [obj]
  (update
   obj
   :thunk
   (fn [thunk]
     (fn []
       (let [{{:keys [minx miny maxx maxy]} :bb, :as drfd} (thunk)
             w (.-width canvas)
             h (.-height canvas)
             dx (max (- minx) (min 0 (- w maxx)))
             dy (max (- miny) (min 0 (- h maxy)))]
         (translate drfd dx dy))))))
