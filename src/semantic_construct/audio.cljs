(ns semantic-construct.audio)

(defn ->Audio [name]
  (js/Audio. (str "assets/audio/" name ".mp3")))

(defn play [s]
  (.play s))

(def current-track (atom nil))

(defn toggle-track! [track]
  (when (not= @current-track track)
    (when-let [ot @current-track]
      (.-pause ot))
    (reset! current-track track))
  (when-let [nt track]
    (if (.-paused nt)
      (do (set! (.-loop nt) true)
          (.play nt))
      (.pause nt))))

(defonce loop1 (->Audio "loop1"))
