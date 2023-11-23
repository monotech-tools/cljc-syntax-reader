
(ns syntax-reader.search.engine
    (:require [syntax-reader.core.prototypes    :as core.prototypes]
              [syntax-reader.interpreter.engine :as interpreter.engine]
              [syntax-reader.search.utils       :as search.utils]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn first-position
  ; @description
  ; Returns the position of the first occurence of the given 'x' regex pattern in the given 'n' string.
  ;
  ; @param (string) n
  ; @param (regex pattern) x
  ; @param (map)(opt) tags
  ; {:comment (vector)(opt)
  ;   [(regex pattern) opening-pattern
  ;    (regex pattern) closing-pattern
  ;    (map)(opt) options
  ;     For available tag options, check out the 'interpreter' function's documentation.]
  ;  Default: [#";" #"\n"]
  ;  :quote (vector)(opt)
  ;   [(regex pattern) opening-pattern
  ;    (regex pattern) closing-pattern
  ;    (map)(opt) options
  ;     For available tag options, check out the 'interpreter' function's documentation.]
  ;  Default: [#"\"" #"\""]}
  ; @param (map)(opt) options
  ; {:endpoint (integer)(opt)
  ;   Quits searching at the given 'endpoint' position in the given 'n' string.
  ;  :ignore-commented? (boolean)(opt)
  ;   Default: true
  ;  :ignore-escaped? (boolean)(opt)
  ;   Default: true
  ;  :ignore-quoted? (boolean)(opt)
  ;   Default: true
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
  ; (first-position ".my-class {/* width: 0 */ width: auto; }" #"width" {:comment [#"/\*" #"\*/"]})
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
          (let [tags (core.prototypes/tags-prototype tags options)]
               (interpreter.engine/interpreter n f0 nil tags options)))))
