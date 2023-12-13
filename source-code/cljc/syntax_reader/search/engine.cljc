
(ns syntax-reader.search.engine
    (:require [syntax-interpreter.api        :as syntax-interpreter]
              [syntax-reader.core.prototypes :as core.prototypes]
              [syntax-reader.search.utils    :as search.utils]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn first-position
  ; @description
  ; - Returns the position of the first occurence of the given 'x' regex pattern in the given 'n' string.
  ; - By default, it ignores commented and quoted parts of the string.
  ;
  ; @param (string) n
  ; @param (regex pattern) x
  ; @param (vectors in vector)(opt)(in decreasing priority order) tags
  ; For more information, check out the documentation of the 'syntax-interpreter.api/interpreter' function.
  ; [[(keyword) tag-name
  ;   (regex pattern) pattern / opening-pattern
  ;   (regex pattern)(opt) closing-pattern
  ;   (map)(opt) tag-options]]
  ; Default:
  ; [[:comment       #"\;"   #"\n" {:disable-interpreter? true}]
  ;  [:meta-string   #"\^\"" #"\"" {:disable-interpreter? true}]
  ;  [:regex-pattern #"\#\"" #"\"" {:disable-interpreter? true}]
  ;  [:string        #"\""   #"\"" {:disable-interpreter? true}]]
  ; @param (map)(opt) options
  ; {:endpoint (integer)(opt)
  ;   Quits searching at the given 'endpoint' position in the given 'n' string.
  ;  :offset (integer)(opt)
  ;   Starts searching at the given 'offset' position in the given 'n' string.
  ;   The returned position is an offset independent absolute value.}
  ;
  ; @example
  ; (first-position ".my-class { width: auto; }" #"width")
  ; =>
  ; 12
  ;
  ; @example
  ; (first-position ".my-class {/* width: 0 */ width: auto; }" #"width" [[:comment #"/\*" #"\*/"]])
  ; =>
  ; 26
  ;
  ; @return (integer)
  ([n x]
   (first-position n x {} {}))

  ([n x tags]
   (first-position n x tags {}))

  ([n x tags options]
   (letfn [; @param (nil) result
           ; @param (map) state
           ; {:cursor (integer)}
           ; @param (map) metafunctions
           ; {:stop (function)}
           ;
           ; @return (nil or vector)
           (f0 [_ {:keys [cursor] :as state} {:keys [stop] :as metafunctions}]
               (if (search.utils/pattern-found? n x state metafunctions)
                   (stop cursor)))]
          ; ...
          (let [tags (core.prototypes/tags-prototype tags)]
               (syntax-interpreter/interpreter n f0 nil tags options)))))
