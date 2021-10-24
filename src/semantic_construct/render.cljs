(ns semantic-construct.render
  (:require-macros [semantic-construct.render :refer [with-ctx-bindings]])
  (:require [semantic-construct.mouse :as mouse]))

(def canvas (. js/document (getElementById "canvas")))
(def ctx (.getContext canvas "2d"))

(defprotocol Transform
  (transform [this matrix]))

;; just don't mutate it, okay?
(def identity-matrix (js/DOMMatrix.))

(defn translation [dx dy]
  (-> (js/DOMMatrix.) (.translateSelf dx dy)))

(defrecord BB [minx miny maxx maxy]
  Transform
  (transform [this mat]
    (let [minp (-> (js/DOMPoint. minx miny) (.matrixTransform mat))
          maxp (-> (js/DOMPoint. maxx maxy) (.matrixTransform mat))]
      (BB. (.-x minp) (.-y minp) (.-x maxp) (.-y maxp)))))
(defrecord InCtxRenderObject [bb mat plot]
  Transform
  (transform [this omat]
    (.preMultiplySelf mat omat)
    (update this :bb transform omat)))
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

(def sprites (. js/document (getElementById "sprites")))
(defn sprite-object [bb-thunk sx sy sw sh]
  (object
   (fn []
     (->InCtxRenderObject
      (bb-thunk)
      (js/DOMMatrix.)
      (fn [this]
        (let [{:keys [x y w h]} (:bb this)]
          (.drawImage ctx
                      sprites
                      sx sy
                      sw sh
                      x y
                      w h)))))))

(defn rect [bb-thunk]
  (object
   (fn []
     (->InCtxRenderObject
      (bb-thunk)
      (js/DOMMatrix.)
      (fn [{{:keys [minx miny maxx maxy]} :bb}]
        ;; the matrix is already captured in the bb
        (.fillRect ctx minx miny (- maxx minx) (- maxy miny)))))))

(defn stroke-rect [bb-thunk]
  (object
   (fn []
    (->InCtxRenderObject
     (bb-thunk)
     (js/DOMMatrix.)
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
         (js/DOMMatrix.)
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
        (js/DOMMatrix.)
        (fn [{:keys [mat]}]
          (run! plot-object (map #(transform % mat) derefd))))))))
