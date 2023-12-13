
(ns syntax-reader.remove.utils
    (:require [fruits.string.api :as string]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn remove-found-tag?
  ; @ignore
  ;
  ; @param (string) result
  ; @param (map) state
  ; @param (map) metafunctions
  ; @param (keyword) found-tag
  ;
  ; @usage
  ; (remove-found-tag? "..." {...} {...} :my-tag)
  ;
  ; @return (boolean)
  [_ _ {:keys [tag-options]} found-tag]
  (-> (tag-options found-tag) :keep? not))

(defn remove-found-tag
  ; @ignore
  ;
  ; @param (string) result
  ; @param (map) state
  ; @param (map) metafunctions
  ; @param (keyword) found-tag
  ;
  ; @usage
  ; (remove-found-tag "..." {...} {...} :my-tag)
  ;
  ; @return (string)
  [result {{:keys [adjust] :or {adjust 0}} :metadata :keys [cursor]} {:keys [tag-started-at]} found-tag]
  (let [tag-started-at (tag-started-at found-tag)]
       (string/cut-range result (- tag-started-at adjust)
                                (- cursor         adjust))))

(defn update-metadata
  ; @ignore
  ;
  ; @param (string) result
  ; @param (map) state
  ; @param (map) metafunctions
  ; @param (keyword) found-tag
  ;
  ; @usage
  ; (update-metadata "..." {...} {...} :my-tag)
  ;
  ; @return (map)
  [_ {{:keys [adjust] :or {adjust 0}} :metadata :keys [cursor]} {:keys [tag-started-at]} found-tag]
  (let [tag-started-at (tag-started-at found-tag)
        cut-length     (- cursor tag-started-at)]
       {:adjust (+ adjust cut-length)}))
