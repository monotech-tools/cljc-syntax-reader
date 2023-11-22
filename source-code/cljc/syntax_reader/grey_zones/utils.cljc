
(ns syntax-reader.grey-zones.utils
    (:require [vector.api :as vector]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn check-if-comment-opens
  ; @ignore
  ;
  ; @description
  ; If a comment opening tag ends at the actual cursor position, it adds the actual cursor position (as ':opened-at' position) to the last commented zone in the result vector.
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
      (-> result (update :commented vector/update-last-item assoc :opened-at cursor))
      (-> result)))

(defn check-if-comment-closes
  ; @ignore
  ;
  ; @description
  ; If a comment closing tag starts at the actual cursor position, it adds the actual cursor position (as ':closed-at' position) to the last commented zone in the result vector.
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
      (-> result (update :commented vector/update-last-item assoc :closed-at cursor))
      (-> result)))

(defn check-if-comment-ends
  ; @ignore
  ;
  ; @description
  ; If a comment closing tag ends at the actual cursor position, it adds the actual cursor position (as ':ended-at' position) to the last commented zone in the result vector.
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
      (-> result (update :commented vector/update-last-item assoc :ended-at cursor))
      (-> result)))

(defn check-if-quote-opens
  ; @ignore
  ;
  ; @description
  ; If a quote opening tag ends at the actual cursor position, it adds the actual cursor position (as ':opened-at' position) to the last quoted zone in the result vector.
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
      (-> result (update :quoted vector/update-last-item assoc :opened-at cursor))
      (-> result)))

(defn check-if-quote-closes
  ; @ignore
  ;
  ; @description
  ; If a quote closing tag starts at the actual cursor position, it adds the actual cursor position (as ':closed-at' position) to the last quoted zone in the result vector.
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
      (-> result (update :quoted vector/update-last-item assoc :closed-at cursor))
      (-> result)))

(defn check-if-quote-ends
  ; @ignore
  ;
  ; @description
  ; If a quote closing tag ends at the actual cursor position, it adds the actual cursor position (as ':ended-at' position) to the last quoted zone in the result vector.
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
      (-> result (update :quoted vector/update-last-item assoc :ended-at cursor))
      (-> result)))

(defn check-if-grey-zone-starts
  ; @ignore
  ;
  ; @description
  ; If a comment / quote opening tag starts at the actual cursor position, it adds the actual cursor position (as ':started-at' position) in a new commented / quoted zone to the result vector.
  ;
  ; @param (map) result
  ; @param (map) state
  ; {:cursor (integer)}
  ; @param (map) metafunctions
  ; {:opening-tag-starts? (function)}
  ;
  ; @return (map)
  [result {:keys [cursor]} {:keys [tag-starts?]}]
  (cond (tag-starts? :comment) (update result :commented vector/conj-item {:started-at cursor})
        (tag-starts? :quote)   (update result :quoted    vector/conj-item {:started-at cursor})
        :return result))
