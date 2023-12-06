
(ns syntax-reader.grey-zones.engine
    (:require [syntax-interpreter.api         :as syntax-interpreter]
              [syntax-reader.core.prototypes  :as core.prototypes]
              [syntax-reader.grey-zones.utils :as grey-zones.utils]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn grey-zones
  ; @description
  ; Returns the ranges (zone boundaries) in the given 'n' string that are commented / quoted and the cursor positions that are escaped.
  ;
  ; @param (string) n
  ; @param (vectors in vector)(opt) tags
  ; Default:
  ; [[:comment #"\;" #"\n" {:disable-interpreter? true}]
  ;  [:quote   #"\"" #"\"" {:disable-interpreter? true}]]
  ; @param (map)(opt) options
  ; {:endpoint (integer)(opt)
  ;   Quits collecting grey zones at the given 'endpoint' position in the given 'n' string.
  ;  :offset (integer)(opt)
  ;   Starts collecting grey zones from the given 'offset' position in the given 'n' string.
  ;   The returned positions are offset independent absolute values.}
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

  ([n tags options]
   (letfn [; @param (map) result
           ; @param (map) state
           ; @param (map) metafunctions
           ;
           ; @return (map)
           ; {:commented (integer pairs in vectors in vector)
           ;  :escaped (integers in vector)
           ;  :quoted (integer pairs in vectors in vector)}
           (f0 [result state metafunctions]
               (-> result (grey-zones.utils/check-if-comment-opens    state metafunctions)
                          (grey-zones.utils/check-if-comment-closes   state metafunctions)
                          (grey-zones.utils/check-if-comment-ends     state metafunctions)
                          (grey-zones.utils/check-if-quote-opens      state metafunctions)
                          (grey-zones.utils/check-if-quote-closes     state metafunctions)
                          (grey-zones.utils/check-if-quote-ends       state metafunctions)
                          (grey-zones.utils/check-if-grey-zone-starts state metafunctions)))]
          ; ...
          (let [initial {:commented [] :escaped? [] :quoted []}
                tags    (core.prototypes/tags-prototype tags)]
               (syntax-interpreter/interpreter n f0 initial tags options)))))
