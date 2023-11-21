
(ns syntax-reader.grey-zones.utils
    (:require [string.api :as string]
              [vector.api :as vector]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn offset-exceeded?
  ; @ignore
  ;
  ; @description
  ; Returns TRUE if the actual cursor value exceeded the given 'offset' value.
  ;
  ; @param (map) result
  ; @param (map) state
  ; {}
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [_ {:keys [cursor]} _])

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn last-comment-not-opened-yet?
  ; @ignore
  ;
  ; @description
  ; Returns TRUE if ...
  ; ... at least one item is already added to the ':commented' vector in the 'result' map,
  ; ... and the last item in that ':commented' vector does not contain the ':opened-at' position (yet).
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [result _ _]
  (and (-> result :commented last map?)
       (-> result :commented last :opened-at nil?)))

(defn last-comment-not-closed-yet?
  ; @ignore
  ;
  ; @description
  ; Returns TRUE if ...
  ; ... at least one item is already added to the ':commented' vector in the 'result' map,
  ; ... and the last item in that ':commented' vector does not contain the ':closed-at' position (yet).
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [result _ _]
  (and (-> result :commented last map?)
       (-> result :commented last :closed-at nil?)))

(defn last-comment-not-ended-yet?
  ; @ignore
  ;
  ; @description
  ; Returns TRUE if ...
  ; ... at least one item is already added to the ':commented' vector in the 'result' map,
  ; ... and the last item in that ':commented' vector does not contain the ':ended-at' position (yet).
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [result _ _]
  (and (-> result :commented last map?)
       (-> result :commented last :ended-at nil?)))

(defn last-quote-not-opened-yet?
  ; @ignore
  ;
  ; @description
  ; Returns TRUE if ...
  ; ... at least one item is already added to the ':quoted' vector in the 'result' map,
  ; ... and the last item in that ':quoted' vector does not contain the ':opened-at' position (yet).
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [result _ _]
  (and (-> result :quoted last map?)
       (-> result :quoted last :opened-at nil?)))

(defn last-quote-not-closed-yet?
  ; @ignore
  ;
  ; @description
  ; Returns TRUE if ...
  ; ... at least one item is already added to the ':quoted' vector in the 'result' map,
  ; ... and the last item in that ':quoted' vector does not contain the ':closed-at' position (yet).
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [result _ _]
  (and (-> result :quoted last map?)
       (-> result :quoted last :closed-at nil?)))

(defn last-quote-not-ended-yet?
  ; @ignore
  ;
  ; @description
  ; Returns TRUE if ...
  ; ... at least one item is already added to the ':quoted' vector in the 'result' map,
  ; ... and the last item in that ':quoted' vector does not contain the ':ended-at' position (yet).
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [result _ _]
  (and (-> result :quoted last map?)
       (-> result :quoted last :ended-at nil?)))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn check-if-comment-opening-tag-ends
  ; @ignore
  ;
  ; @description
  ; If a comment opening tag ends at the actual cursor position, it adds the actual cursor position (as ':opened-at' position) to the last commented zone in the result vector.
  ;
  ; @param (map) result
  ; @param (map) state
  ; {:cursor (integer)}
  ; @param (map) metafunctions
  ; {:opening-tag-ends? (function)}
  ;
  ; @return (map)
  [result {:keys [cursor]} {:keys [opening-tag-ends?]}]
  (if (-> :comment opening-tag-ends?)
      (-> result (update :commented vector/update-last-item assoc :opened-at cursor))
      (-> result)))

(defn check-if-comment-closing-tag-starts
  ; @ignore
  ;
  ; @description
  ; If a comment closing tag starts at the actual cursor position, it adds the actual cursor position (as ':closed-at' position) to the last commented zone in the result vector.
  ;
  ; @param (map) result
  ; @param (map) state
  ; {:cursor (integer)}
  ; @param (map) metafunctions
  ; {:closing-tag-starts? (function)}
  ;
  ; @return (map)
  [result {:keys [cursor]} {:keys [closing-tag-starts?]}]
  (if (-> :comment closing-tag-starts?)
      (-> result (update :commented vector/update-last-item assoc :closed-at cursor))
      (-> result)))

(defn check-if-comment-closing-tag-ends
  ; @ignore
  ;
  ; @description
  ; If a comment closing tag ends at the actual cursor position, it adds the actual cursor position (as ':ended-at' position) to the last commented zone in the result vector.
  ;
  ; @param (map) result
  ; @param (map) state
  ; {:cursor (integer)}
  ; @param (map) metafunctions
  ; {:closing-tag-ends? (function)}
  ;
  ; @return (map)
  [result {:keys [cursor]} {:keys [closing-tag-ends?]}]
  (if (-> :comment closing-tag-ends?)
      (-> result (update :commented vector/update-last-item assoc :ended-at cursor))
      (-> result)))

(defn check-if-quote-opening-tag-ends
  ; @ignore
  ;
  ; @description
  ; If a quote opening tag ends at the actual cursor position, it adds the actual cursor position (as ':opened-at' position) to the last quoted zone in the result vector.
  ;
  ; @param (map) result
  ; @param (map) state
  ; {:cursor (integer)}
  ; @param (map) metafunctions
  ; {:opening-tag-ends? (function)}
  ;
  ; @return (map)
  [result {:keys [cursor]} {:keys [opening-tag-ends?]}]
  (if (-> :quote opening-tag-ends?)
      (-> result (update :quoted vector/update-last-item assoc :opened-at cursor))
      (-> result)))

(defn check-if-quote-closing-tag-starts
  ; @ignore
  ;
  ; @description
  ; If a quote closing tag starts at the actual cursor position, it adds the actual cursor position (as ':closed-at' position) to the last quoted zone in the result vector.
  ;
  ; @param (map) result
  ; @param (map) state
  ; {:cursor (integer)}
  ; @param (map) metafunctions
  ; {:closing-tag-starts? (function)}
  ;
  ; @return (map)
  [result {:keys [cursor]} {:keys [closing-tag-starts?]}]
  (if (-> :quote closing-tag-starts?)
      (-> result (update :quoted vector/update-last-item assoc :closed-at cursor))
      (-> result)))

(defn check-if-quote-closing-tag-ends
  ; @ignore
  ;
  ; @description
  ; If a quote closing tag ends at the actual cursor position, it adds the actual cursor position (as ':ended-at' position) to the last quoted zone in the result vector.
  ;
  ; @param (map) result
  ; @param (map) state
  ; {:cursor (integer)}
  ; @param (map) metafunctions
  ; {:closing-tag-ends? (function)}
  ;
  ; @return (map)
  [result {:keys [cursor]} {:keys [closing-tag-ends?]}]
  (if (-> :quote closing-tag-ends?)
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
  [result {:keys [cursor]} {:keys [opening-tag-starts?]}]
  (cond (opening-tag-starts? :comment) (update result :commented vector/conj-item {:started-at cursor})
        (opening-tag-starts? :quote)   (update result :quoted    vector/conj-item {:started-at cursor})
        :return result))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn remove-commented-part
  ; @ignore
  ;
  ; @description
  ; ...
  ;
  ; @param (string) result
  ; @param (integer) adjust
  ; @param (map) commented-zone
  ; {:ended-at (integer)
  ;  :started-at (integer)}
  ; @param (map) options
  ; {:keep-indents? (boolean)(opt)
  ;  :remove-leftover-blank-lines? (boolean)(opt)}
  [result adjust {:keys [started-at ended-at]} {:keys [keep-indents? remove-leftover-blank-lines?]}]
  (let [zone-start (-> started-at (- adjust))
        zone-end   (-> ended-at   (- adjust))]
       (as-> result % ; Removing the commented part ...
                      (string/cut-range % zone-start zone-end {:keep-inline-position? keep-indents?})
                      ; Removing leftover blank line (if any) ...
                      (cond-> % (and remove-leftover-blank-lines? (string/in-blank-line? % zone-start))
                                (string/remove-containing-line zone-start)))))
