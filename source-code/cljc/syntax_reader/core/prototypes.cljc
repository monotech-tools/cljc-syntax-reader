
(ns syntax-reader.core.prototypes
    (:require [syntax-reader.core.config :as core.config]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn comment-tag-prototype
  ; @ignore
  ;
  ; @description
  ; - Uses the default value of the ':comment' / ':quote' tag, in case the tag is not provided in the given 'tags' map.
  ; - Ensures that the ':disable-interpreter?' option is TRUE for the ':comment' / ':quote' tag, (even if the tag is provided in the given 'tags' map).
  ;
  ; @param (map) tags
  ; {:comment (vector)}
  ;
  ; @return (vector)
  ; [(regex-pattern) opening-tag
  ;  (regex-pattern) closing-tag
  ;  (map) options]
  [tags]
  [(or (-> tags                     :comment first)
       (-> core.config/DEFAULT-TAGS :comment first))
   (or (-> tags                     :comment second)
       (-> core.config/DEFAULT-TAGS :comment second))
   {:disable-interpreter? true}])

(defn comment-tag-prototype
  ; @ignore
  ;
  ; @description
  ; - Uses the default value of the ':comment' / ':quote' tag, in case the tag is not provided in the given 'tags' map.
  ; - Ensures that the ':disable-interpreter?' option is TRUE for the ':comment' / ':quote' tag, (even if the tag is provided in the given 'tags' map).
  ;
  ; @param (map) tags
  ; {:comment (vector)
  ;  :quote (vector)}
  ;
  ; @return (map)
  ; {:comment (vector)
  ;  :quote (vector)}
  [tags]
  {:comment [(or (-> tags                     :comment first)
                 (-> core.config/DEFAULT-TAGS :comment first))
             (or (-> tags                     :comment second)
                 (-> core.config/DEFAULT-TAGS :comment second))
             {:disable-interpreter? true}]
   :quote   [(or (-> tags                     :quote first)
                 (-> core.config/DEFAULT-TAGS :quote first))
             (or (-> tags                     :quote second)
                 (-> core.config/DEFAULT-TAGS :quote second))
             {:disable-interpreter? true}]})
