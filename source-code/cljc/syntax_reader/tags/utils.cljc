
(ns syntax-reader.tags.utils)

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn opening-tag-found?
  ; @ignore
  ;
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [_ {:keys []}])

(defn closing-tag-found?
  ; @ignore
  ;
  ; @param (map) state
  ; @param (map) metafunctions
  ; {:closing-tag-starts? (function)
  ;  :tag-actual-depth (function)}
  ;
  ; @return (boolean)
  [_ {:keys [closing-tag-starts? tag-actual-depth]}]
  (and (-> :$searched-tag closing-tag-starts?)
       (-> :$searched-tag tag-actual-depth (= 1))))
