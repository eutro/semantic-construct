(ns semantic-construct.include
  (:import (java.io PushbackReader))
  (:require [clojure.java.io :as io]))

(defn read-src [src]
  (with-open [is (-> (str "public/" src)
                     io/resource
                     (or (throw (ex-info "No such resource for include." {:src src})))
                     io/reader
                     PushbackReader.)]
    (read is)))

(defn read-srcs [srcs]
  (mapv read-src srcs))

(defmacro include [src]
  (read-src src))

(defmacro includev [srcs]
  (read-srcs srcs))
