(ns semantic-construct.state)

(def state (atom {:screen {:type :load}}))

(defrecord Escape [])
(defn escape! []
  (throw (->Escape)))

(defn call-with-escape-base [f]
  (try (f) (catch Escape _)))
