
(ns syntax-reader.grey-zones.engine
    (:require [seqable.api                      :as seqable]
              [syntax-reader.core.prototypes    :as core.prototypes]
              [syntax-reader.grey-zones.utils   :as grey-zones.utils]
              [syntax-reader.interpreter.engine :as interpreter.engine]
              [vector.api                       :as vector]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn grey-zones
  ; @description
  ; - Returns the ranges (zone boundaries) in the given 'n' string that are commented / quoted and the cursor positions that are escaped.
  ; - If the 'offset' parameter is passed, the search starts from the offset position.
  ; - The returned positions are absolute values and they are independent from the given offset value.
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
  ; {:commented [{:started-at 10 :opened-at 11 :closed-at 22 :ended-at 23}]
  ;  :quoted    [{:started-at 26 :opened-at 27 :closed-at 35 :ended-at 36}]}
  ;
  ; @example
  ; (grey-zones "My string ; My comment 1\n ; My comment 2\n")
  ; =>
  ; {:commented [{:started-at 10 :opened-at 11 :closed-at 24 :ended-at 25}
  ;              {:started-at 27 :opened-at 28 :closed-at 41 :ended-at 42}]}
  ;
  ; @return (map)
  ; {:commented (maps in vector)
  ;   [{:started-at (integer)
  ;     :opened-at (integer)
  ;     :closed-at (integer)
  ;     :ended-at (integer)}]
  ;  :escaped (integers in vector)
  ;   [(integer) escaped-position]
  ;  :quoted (maps in vector)
  ;   [{:started-at (integer)
  ;     :opened-at (integer)
  ;     :closed-at (integer)
  ;     :ended-at (integer)}]}
  ([n]
   (grey-zones n {} {}))

  ([n tags]
   (grey-zones n tags {}))

  ([n tags {:keys [endpoint offset] :as options :or {endpoint (count n) offset 0}}]
   (letfn [; @description
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
           (f0 [result {:keys [cursor] :as state} {:keys [closing-tag-ends? closing-tag-starts? opening-tag-ends? opening-tag-starts? stop] :as metafunctions}]
               (cond ; - If the 'offset' position is passed, it starts collecting grey zones from the given 'offset' position,
                     ;   but it lets the interpreter start from the 0th cursor position in order to make accurate tag map.
                     ; - If the 'endpoint' position is passed, it quits collecting grey zones at the given 'endpoint' position,
                     ;   and it uses the 'stop' metafunction to stop the interpreter.
                     (-> cursor (< offset))   (-> result)
                     (-> cursor (> endpoint)) (-> result stop)
                     ; ...
                     (grey-zones.utils/last-comment-not-opened-yet? result state metafunctions) (grey-zones.utils/check-if-comment-opening-tag-ends   result state metafunctions)
                     (grey-zones.utils/last-comment-not-closed-yet? result state metafunctions) (grey-zones.utils/check-if-comment-closing-tag-starts result state metafunctions)
                     (grey-zones.utils/last-comment-not-ended-yet?  result state metafunctions) (grey-zones.utils/check-if-comment-closing-tag-ends   result state metafunctions)
                     (grey-zones.utils/last-quote-not-opened-yet?   result state metafunctions) (grey-zones.utils/check-if-quote-opening-tag-ends     result state metafunctions)
                     (grey-zones.utils/last-quote-not-closed-yet?   result state metafunctions) (grey-zones.utils/check-if-quote-closing-tag-starts   result state metafunctions)
                     (grey-zones.utils/last-quote-not-ended-yet?    result state metafunctions) (grey-zones.utils/check-if-quote-closing-tag-ends     result state metafunctions)
                     ; ...
                     :return (grey-zones.utils/check-if-grey-zone-starts result state metafunctions)))]
          ; ...
          (let [initial {:commented [] :escaped? [] :quoted []}
                tags    {:comment (core.prototypes/comment-tag-prototype tags)
                         :quote   (core.prototypes/quote-tag-prototype   tags)}]
               (interpreter.engine/interpreter n f0 initial tags options)))))

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
  ;   Default: false
  ;  :offset (integer)(opt)
  ;   Starts searching at the offset position in the given 'n' string.
  ;   Default: 0
  ;  :remove-leftover-blank-lines? (boolean)(opt)
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
  ;                         {:comment [#"/\*" #"\*/"})
  ; =>
  ; "body {  color: blue; }"
  ;
  ; @return (string)
  ([n]
   (remove-commented-parts n {} {}))

  ([n tags]
   (remove-commented-parts n tags {}))

  ([n tags options]
   (let [grey-zones (grey-zones n tags options)]
        (letfn [; - If there is at least one commented zone left in the 'commented-zones' vector,
                ;   it uses the first commented zone's boundaries to cut out the first commented zone from the 'result' string
                ;   and calls itself recursivelly (while dropping the first zone from the 'commented-zones' vector).
                ; - If there is no more commented zone left in the 'commented-zones' vector it returns the result.
                ; - It uses the actual 'adjust' value to determine how many characters are already cut out from the result
                ;   and to adjust the cutting boundaries in every iteration.
                (f1 [result adjust [commented-zone _ :as commented-zones]]
                    (if commented-zone (let [updated-result (grey-zones.utils/remove-commented-part result adjust commented-zone options)
                                             updated-adjust (+ adjust (seqable/count-difference result updated-result))]
                                            (f1 updated-result updated-adjust (vector/remove-first-item commented-zones)))
                                       (-> result)))]
               ; ...
               (f1 n 0 (:commented grey-zones))))))
