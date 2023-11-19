
(ns syntax-reader.grey-zones
    (:require [map.api                   :as map]
              [seqable.api               :as seqable]
              [string.api                :as string]
              [syntax-reader.core.config      :as core.config]
              [syntax-reader.interpreter.core :as interpreter.core]
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
  ;   [(regex pattern) opening-tag
  ;    (regex pattern) closing-tag
  ;    (map)(opt) tag-options
  ;     {:disable-interpreter? (boolean)(opt)}
  ;       Default: false}]
  ;  Default: [#";" #"\n" {:disable-interpreter? true}]
  ;  :quote (vector)(opt)
  ;   [(regex pattern) opening-tag
  ;    (regex pattern) closing-tag
  ;    (map)(opt) tag-options
  ;     {:disable-interpreter? (boolean)(opt)}
  ;       Default: false}]}
  ;  Default: [#"\"" #"\"" {:disable-interpreter? true}]
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
   (letfn [; @description
           ; - Uses the default value of the ':comment' / ':quote' tag, if the tag is not provided in the given 'tags' map.
           ; - Makes sure that the ':disable-interpreter?' option is TRUE for the ':comment' / ':quote' tag, (even if the tag is provided in the given 'tags' map).
           ;
           ; @return (map)
           (f0 []
               (-> tags (-> (map/reversed-merge {:comment (-> core.config/DEFAULT-TAGS :comment)})
                            (assoc-in           [:comment 2 :disable-interpreter?] true))
                        (-> (map/reversed-merge {:quote   (-> core.config/DEFAULT-TAGS :quote)})
                            (assoc-in           [:quote   2 :disable-interpreter?] true))))

           ; @description
           ; Returns TRUE if ...
           ; ... there is at least one item already added to the ':commented' vector in the 'result' map,
           ; ... and the last item in the ':commented' vector only contains a start boundary (missing end boundary = still opened comment).
           ;
           ; @param (map) result
           ;
           ; @return (boolean)
           (f2 [result]
               (and (-> result :commented last vector?)
                    (-> result :commented last second nil?)))

           ; @description
           ; Returns TRUE if ...
           ; ... there is at least one item already added to the ':quoted' vector in the 'result' map,
           ; ... and the last item in the ':quoted' vector only contains a start boundary (missing end boundary = still opened quote).
           ;
           ; @param (map) result
           ;
           ; @return (boolean)
           (f3 [result]
               (and (-> result :quoted last vector?)
                    (-> result :quoted last second nil?)))

           ; @description
           ; Adds the actual cursor position (as end boundary) to the last item in the ':commented' vector in the 'result' map.
           ;
           ; @param (map) result
           ; {:commented (vector)}
           ; @param (map) state
           ; {:cursor (integer)}
           ; @param (map) metafunctions
           ;
           ; @return (map)
           (f4 [{:keys [commented] :as result} {:keys [cursor]} _]
               (update-in result [:commented (seqable/last-dex commented)] vector/conj-item (dec cursor)))

           ; @description
           ; Adds the actual cursor position (as end boundary) to the last item in the ':quoted' vector in the 'result' map.
           ;
           ; @param (map) result
           ; {:quoted (vector)}
           ; @param (map) state
           ; {:cursor (integer)}
           ; @param (map) metafunctions
           ;
           ; @return (map)
           (f5 [{:keys [quoted] :as result} {:keys [cursor]} _]
               (update-in result [:quoted (seqable/last-dex quoted)] vector/conj-item (dec cursor)))

           ; @description
           ; ...
           ;
           ; @param (map) result
           ; @param (map) state
           ; @param (map) metafunctions
           ; {:tag-opened (function)}
           ;
           ; @return (map)
           ; {:commented (integer pairs in vectors in vector)
           ;  :escaped (integers in vector)
           ;  :quoted (integer pairs in vectors in vector)}
           (f6 [result {:keys [cursor] :as state} {:keys [tag-not-opened? tag-opened?] :as metafunctions}]
               ; The interpreter starts processing at the 0th cursor position in order to make accurate tag map,
               ; and this ('grey-zones') function starts collecting grey zones from the given 'offset' position.
               (if-not (-> cursor (>= offset))
                       (-> result)
                       (cond ; If the last item is opened in the ':commented' vector in the 'result' map ...
                             ; ... and if the actual cursor position is not commented, that means the commented zone
                             ;     ended at the previous cursor position, so its time to store the end boundary.
                             ; ... and if the actual cursor position is still commented, it returns the unchanged 'result' map.
                             (-> result f2)
                             (if (tag-not-opened? :comment)
                                 (f4 result state metafunctions)
                                 (-> result))
                             ; If the last item is opened in the ':quoted' vector in the 'result' map ...
                             ; ... and if the actual cursor position is not quoted, that means the quoted zone
                             ;     ended at the previous cursor position, so its time to store the end boundary.
                             ; ... and if the actual cursor position is still quoted, it returns the unchanged 'result' map.
                             (-> result f3)
                             (if (tag-not-opened? :quoted)
                                 (f5 result state metafunctions)
                                 (-> result))
                             ; If a commented zone starts at the actual cursor position ...
                             ; ... it updates the 'result' map by adding a vector with the start boundary (only) of the found commented zone.
                             (tag-opened? :comment)
                             (update result :commented vector/conj-item [cursor])
                             ; If a quoted zone starts at the actual cursor position ...
                             ; ... it updates the 'result' map by adding a vector with the start boundary (only) of the found quoted zone.
                             (tag-opened? :quote)
                             (update result :quoted vector/conj-item [cursor])
                             ; ...
                             :return result)))]
          ; ...
          (let [initial {:commented [] :escaped? [] :quoted []}
                options (dissoc options :offset)
                tags    (f0)]
               (interpreter.core/interpreter n f6 initial tags options)))))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn remove-commented-parts
  ; @description
  ; Removes the commented parts from the given 'n' string.
  ;
  ; @param (string) n
  ; @param (vectors in map)(opt) tags
  ; {:comment (vector)(opt)
  ;   [(regex pattern) opening-tag
  ;    (regex pattern) closing-tag
  ;    (map)(opt) tag-options
  ;     {:disable-interpreter? (boolean)(opt)}
  ;       Default: false}]
  ;  Default: [#";" #"\n" {:disable-interpreter? true}]
  ;  :quote (vector)(opt)
  ;   [(regex pattern) opening-tag
  ;    (regex pattern) closing-tag
  ;    (map)(opt) tag-options
  ;     {:disable-interpreter? (boolean)(opt)}
  ;       Default: false}]}
  ;  Default: [#"\"" #"\"" {:disable-interpreter? true}]
  ; @param (map)(opt) options
  ; {:endpoint (integer)(opt)
  ;   Quits searching at the endpoint position in the given 'n' string.
  ;  :ignore-escaped? (boolean)(opt)
  ;   Default: true
  ;  :keep-indents? (boolean)(opt)
  ;   TODO
  ;   Default: false
  ;  :offset (integer)(opt)
  ;   Starts searching at the offset position in the given 'n' string.
  ;   Default: 0
  ;  :remove-leftover-blank-lines? (boolean)(opt)
  ;   TODO
  ;   Default: false
  ;  :trim-line-ends? (boolean)(opt)
  ;   TODO
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
  ;                         {:comment [#"/\*" #"\*/" {:disable-interpreter? true}]})
  ; =>
  ; "body {  color: blue; }"
  ;
  ; @return (string)
  ([n]
   (remove-commented-parts n {} {}))

  ([n tags]
   (remove-commented-parts n tags {}))

  ([n tags {:keys [keep-indents? remove-leftover-blank-lines?] :as options}]
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
