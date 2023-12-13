
(ns syntax-reader.read.utils
    (:require [fruits.vector.api :as vector]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn read-found-tag
  ; @ignore
  ;
  ; @param (string) result
  ; @param (map) state
  ; @param (map) metafunctions
  ; @param (keyword) found-tag
  ;
  ; @usage
  ; (read-found-tag "..." {...} {...} :my-tag)
  ;
  ; @return (map)
  [result _ {:keys [tag-body]} found-tag]
  (let [tag-body (tag-body found-tag)]
       (update result found-tag vector/conj-item tag-body)))
