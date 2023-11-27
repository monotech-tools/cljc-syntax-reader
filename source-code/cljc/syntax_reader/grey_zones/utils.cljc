
(ns syntax-reader.grey-zones.utils
    (:require [map.api     :refer [assoc-by]]
              [seqable.api :refer [last-dex]]
              [vector.api  :as vector]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn check-if-comment-opens
  ; @ignore
  ;
  ; @description
  ; If a comment ends at the actual cursor position, it adds the actual cursor position (as ':opened-at' position) to the last commented zone in the result vector.
  ;
  ; @param (map) result
  ; @param (map) state
  ; {:cursor (integer)}
  ; @param (map) metafunctions
  ; {:tag-opens? (function)}
  ;
  ; @return (map)
  [result {:keys [cursor]} {:keys [tag-opens?]}]
  (if (-> :comment tag-opens?)
      (-> result (assoc-by [:commented last-dex :opened-at] cursor))
      (-> result)))

(defn check-if-comment-closes
  ; @ignore
  ;
  ; @description
  ; If a comment starts at the actual cursor position, it adds the actual cursor position (as ':closed-at' position) to the last commented zone in the result vector.
  ;
  ; @param (map) result
  ; @param (map) state
  ; {:cursor (integer)}
  ; @param (map) metafunctions
  ; {:tag-closes? (function)}
  ;
  ; @return (map)
  [result {:keys [cursor]} {:keys [tag-closes?]}]
  (if (-> :comment tag-closes?)
      (-> result (assoc-by [:commented last-dex :closed-at] cursor))
      (-> result)))

(defn check-if-comment-ends
  ; @ignore
  ;
  ; @description
  ; If a comment ends at the actual cursor position, it adds the actual cursor position (as ':ended-at' position) to the last commented zone in the result vector.
  ;
  ; @param (map) result
  ; @param (map) state
  ; {:cursor (integer)}
  ; @param (map) metafunctions
  ; {:tag-ends? (function)}
  ;
  ; @return (map)
  [result {:keys [cursor]} {:keys [tag-ends?]}]
  (if (-> :comment tag-ends?)
      (-> result (assoc-by [:commented last-dex :ended-at] cursor))
      (-> result)))

(defn check-if-quote-opens
  ; @ignore
  ;
  ; @description
  ; If a quote ends at the actual cursor position, it adds the actual cursor position (as ':opened-at' position) to the last quoted zone in the result vector.
  ;
  ; @param (map) result
  ; @param (map) state
  ; {:cursor (integer)}
  ; @param (map) metafunctions
  ; {:tag-opens? (function)}
  ;
  ; @return (map)
  [result {:keys [cursor]} {:keys [tag-opens?]}]
  (if (-> :quote tag-opens?)
      (-> result (assoc-by [:quoted last-dex :opened-at] cursor))
      (-> result)))

(defn check-if-quote-closes
  ; @ignore
  ;
  ; @description
  ; If a quote starts at the actual cursor position, it adds the actual cursor position (as ':closed-at' position) to the last quoted zone in the result vector.
  ;
  ; @param (map) result
  ; @param (map) state
  ; {:cursor (integer)}
  ; @param (map) metafunctions
  ; {:tag-closes? (function)}
  ;
  ; @return (map)
  [result {:keys [cursor]} {:keys [tag-closes?]}]
  (if (-> :quote tag-closes?)
      (-> result (assoc-by [:quoted last-dex :closed-at] cursor))
      (-> result)))

(defn check-if-quote-ends
  ; @ignore
  ;
  ; @description
  ; If a quote ends at the actual cursor position, it adds the actual cursor position (as ':ended-at' position) to the last quoted zone in the result vector.
  ;
  ; @param (map) result
  ; @param (map) state
  ; {:cursor (integer)}
  ; @param (map) metafunctions
  ; {:tag-ends? (function)}
  ;
  ; @return (map)
  [result {:keys [cursor]} {:keys [tag-ends?]}]
  (if (-> :quote tag-ends?)
      (-> result (assoc-by [:quoted last-dex :ended-at] cursor))
      (-> result)))

(defn check-if-grey-zone-starts
  ; @ignore
  ;
  ; @description
  ; If a comment / quote starts at the actual cursor position, it adds the actual cursor position (as ':started-at' position) in a new commented / quoted zone to the result vector.
  ;
  ; @param (map) result
  ; @param (map) state
  ; {:cursor (integer)}
  ; @param (map) metafunctions
  ; {:tag-starts? (function)}
  ;
  ; @return (map)
  [result {:keys [cursor]} {:keys [tag-starts?]}]
  (cond (tag-starts? :comment) (update result :commented vector/conj-item {:started-at cursor})
        (tag-starts? :quote)   (update result :quoted    vector/conj-item {:started-at cursor})
        :return result))
