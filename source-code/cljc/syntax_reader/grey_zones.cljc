
(ns syntax-reader.grey-zones
    (:require [map.api                   :as map]
              [seqable.api               :as seqable]
              [string.api                :as string]
              [syntax-reader.config      :as config]
              [syntax-reader.interpreter :as interpreter]
              [vector.api                :as vector]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn grey-zones
  ; @description
  ; - Returns the ranges (zone boundaries) in the given 'n' string that are commented / quoted and the cursor positions that are escaped.
  ; - If the 'offset' parameter is passed, the search starts from the offset position.
  ; - The returned positions are absolute values and they are independent from the offset value.
  ;
  ; @param (string) n
  ; @param (vectors in map)(opt) tags
  ; {:comment (vector)(opt)
  ;   [(string) opening-tag
  ;    (string) closing-tag
  ;    (map)(opt) tag-options
  ;     {:disable-interpreter? (boolean)(opt)}
  ;       Default: false}]
  ;  Default: [";" "\n" {:disable-interpreter? true}]
  ;  :quote (vector)(opt)
  ;   [(string) opening-tag
  ;    (string) closing-tag
  ;    (map)(opt) tag-options
  ;     {:disable-interpreter? (boolean)(opt)}
  ;       Default: false}]}
  ;  Default: ["\"" "\"" {:disable-interpreter? true}]
  ; @param (map)(opt) options
  ; {:endpoint (integer)(opt)
  ;   Quits searching at the endpoint position in the given 'n' string.
  ;  :ignore-escaped? (boolean)(opt)
  ;   Default: true
  ;  :offset (integer)(opt)
  ;   Starts searching at the offset position in the given 'n' string.
  ;   Default: 0}
  ;
  ; @usage
  ; (grey-zones "My string" {...})
  ;
  ; @usage
  ; (grey-zones "My string ; My comment\n \"My quote\"")
  ;
  ; @example
  ; (grey-zones "My string ; My comment\n \"My quote\"")
  ; =>
  ; {:commented [[11 22]] :quoted [[27 35]]}
  ;
  ; @example
  ; (grey-zones "My string ; My comment 1\n ; My comment 2\n")
  ; =>
  ; {:commented [[11 24] [28 41]]
  ;
  ; @return (map)
  ; {:commented (integer pairs in vectors in vector)
  ;   [[(integer) zone-start
  ;     (integer) zone-end]]
  ;  :escaped (integers in vector)
  ;   [(integer) escaped-position]
  ;  :quoted (integer pairs in vectors in vector)
  ;   [[(integer) zone-start
  ;     (integer) zone-end]]}
  ([n]
   (grey-zones n {} {}))

  ([n tags]
   (grey-zones n tags {}))

  ([n tags {:keys [offset] :as options :or {offset 0}}]
   (let [; - It uses the default value of the ':comment' / ':quote' tag, if it is not provided in the given 'tags' map.
         ; - It makes sure that the ':disable-interpreter?' option is TRUE for the ':comment' / ':quote' tag, (even if it is provided in the given 'tags' map).
         tags (cond-> tags :comment (map/reversed-merge {:comment (-> config/DEFAULT-TAGS :comment)})
                           :comment (assoc-in           [:comment 2 :disable-interpreter?] true)
                           :quote   (map/reversed-merge {:quote   (-> config/DEFAULT-TAGS :quote)})
                           :quote   (assoc-in           [:quote   2 :disable-interpreter?] true))]
        (letfn [; @description
                ; Returns whether the actual cursor position is commented / quoted.
                ;
                ; @example
                ; (grey-zones "My string ; My comment\n \"My quote\"")
                ;
                ; (f0 {:cursor 13 :tag-map [[:comment 1 11]]}) ; <- A ':comment' tag opened at the 11th cursor position.
                ; =>
                ; :commented
                (f0 [{:keys [tag-map]}]
                    (some (fn [[tag-name _ _]] (case tag-name :comment :commented :quote :quoted nil))
                          (-> tag-map)))

                ;...
                (f1 [{:keys [commented quoted] :as result} {:keys [cursor tag-map] :as state} _]
                    (let [zone-name (f0 state)]
                         (cond ; The interpreter starts the process at the 0th cursor position in order to make accurate tag map,
                               ; and this ('grey-zones') function starts collecting grey zones from the given 'offset' position.
                               (-> offset (> cursor))
                               (-> result)
                               ; If there is at least one vector in the ':commented' vector in the output map,
                               ; and it doesn't contain a second value (missing end boundary = still opened comment) ...
                               (and (-> result :commented last vector?)
                                    (-> result :commented last second nil?))
                               (if (not= :commented zone-name)
                                   ; ... and if the actual cursor position is not commented, that means the commented zone
                                   ; ended at the previous cursor position, so its time to store the end boundary.
                                   (update-in result [:commented (seqable/last-dex commented)] vector/conj-item (dec cursor))
                                   ; ... and if the actual cursor position is still commented, it returns the unchanged output map.
                                   (-> result))

                               ; If there is at least one vector in the ':quoted' vector in the output map,
                               ; and it doesn't contain a second value (missing end boundary = still opened quote) ...
                               (and (-> result :quoted last vector?)
                                    (-> result :quoted last second nil?))
                               (if (not= :quoted zone-name)
                                   ; ... and if the actual cursor position is not quoted, that means the quoted zone
                                   ;     ended at the previous cursor position, so its time to store the end boundary.
                                   (update-in result [:quoted (seqable/last-dex quoted)] vector/conj-item (dec cursor))
                                   ; ... and if the actual cursor position is still commented, it returns the unchanged output map.
                                   (-> result))

                               ; If none of the previous conditions are match, that means no commented / quoted zone is opened
                               ; at the actual cursor position, so it checks whether a commented / quoted zone opens at the actual cursor position ...
                               (-> zone-name)
                               ; ... if yes (a commented / quoted zone opens at the actual cursor position), it updates the output map by adding a vector
                               ;     with the start boundary (only) of the found commented / quoted zone.
                               (update result zone-name vector/conj-item [cursor])

                               ; ...
                               :return result)))]

               ; ...
               (let [initial {:commented [] :escaped? [] :quoted []}]
                    ; The 'offset' parameter is handled by this ('grey-zones') function (not by the 'interpreter' function),
                    ; because the interpreter must start on the 0th cursor position in order to make accurate tag map for comments and quotes.
                    (interpreter/interpreter n f1 initial tags (dissoc options :offset)))))))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn remove-commented-parts
  ; @description
  ; Removes the commented parts from the given 'n' string.
  ;
  ; @param (string) n
  ; @param (vectors in map)(opt) tags
  ; {:comment (vector)(opt)
  ;   [(string) opening-tag
  ;    (string) closing-tag
  ;    (map)(opt) tag-options
  ;     {:disable-interpreter? (boolean)(opt)}
  ;       Default: false}]
  ;  Default: [";" "\n" {:disable-interpreter? true}]
  ;  :quote (vector)(opt)
  ;   [(string) opening-tag
  ;    (string) closing-tag
  ;    (map)(opt) tag-options
  ;     {:disable-interpreter? (boolean)(opt)}
  ;       Default: false}]}
  ;  Default: ["\"" "\"" {:disable-interpreter? true}]
  ; @param (map)(opt) options
  ; {:endpoint (integer)(opt)
  ;   Quits searching at the endpoint position in the given 'n' string.
  ;  :fix-indents? (boolean)(opt)
  ;   Default: false
  ;  :ignore-escaped? (boolean)(opt)
  ;   Default: true
  ;  :offset (integer)(opt)
  ;   Starts searching at the offset position in the given 'n' string.
  ;   Default: 0
  ;  :remove-leftover-newlines? (boolean)(opt)
  ;   Default: false}
  ;
  ; @usage
  ; (remove-commented-parts "(defn my-function [])\n ; My comment\n")
  ;
  ; @example
  ; (remove-commented-parts "(defn my-function [])\n ; My comment\n")
  ; =>
  ; "(defn my-function [])\n "
  ;
  ; @example
  ; (remove-commented-parts "body { /* My comment */ color: blue; }"
  ;                         {:comment ["/*" "*/" {:disable-interpreter? true}]})
  ; =>
  ; "body {  color: blue; }"
  ;
  ; @return (string)
  ([n]
   (remove-commented-parts n {} {}))

  ([n tags]
   (remove-commented-parts n tags {}))

  ([n tags {:keys [fix-indents? remove-leftover-newlines?] :as options}]
   (let [grey-zones (grey-zones n tags options)]
        (letfn [; - If there is at least one commented zone left in the 'commented-zones' vector,
                ;   it uses the first commented zone's boundaries to cut out that zone from the 'result' string
                ;   and calls itself recursivelly (while dropping the first zone from the 'commented-zones' vector).
                ; - If there is no more commented zone left in the 'commented-zones' vector it returns the result.
                ; - It uses the actual offset value to determine how many characters are already cut out from the result
                ;   and to adjust the cutting boundaries in every iteration.
                (f4 [result offset [[zone-start zone-end :as commented-zone] _ :as commented-zones]]
                    (if commented-zone (let [opening-tag-length (-> tags :comment first  count)
                                             closing-tag-length (-> tags :comment second count)
                                             zone-start (-> zone-start (- offset) (- opening-tag-length))
                                             zone-end   (-> zone-end   (- offset) (+ closing-tag-length))
                                             cut-length (-> zone-end   (- zone-start))]
                                            (f4 (string/cut-range result zone-start zone-end)
                                                (+ offset cut-length)
                                                (vector/remove-first-item commented-zones)))
                                       (-> result)))]
               ; ...
               (f4 n 0 (:commented grey-zones))))))
