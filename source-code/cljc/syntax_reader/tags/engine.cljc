
(ns syntax-reader.tags.engine
    (:require [syntax-reader.core.prototypes    :as core.prototypes]
              [syntax-reader.interpreter.engine :as interpreter.engine]
              [syntax-reader.tags.utils         :as tags.utils]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn opening-match-position
  ; @description
  ; Returns the position of the first match of the given 'opening-pattern' regex pattern in the given 'n' string.
  ;
  ; @param (string) n
  ; @param (regex pattern) opening-pattern
  ; @param (regex pattern) closing-pattern
  ; @param (map)(opt) tags
  ; {:comment (vector)(opt)
  ;   [(regex pattern) pattern / opening-pattern
  ;    (regex pattern)(opt) closing-pattern
  ;    (map)(opt) options
  ;     For available tag options, check out the 'interpreter' function's documentation.]
  ;  Default: [#";.*\n"]
  ;  :quote (vector)(opt)
  ;   [(regex pattern) pattern / opening-pattern
  ;    (regex pattern)(opt) closing-pattern
  ;    (map)(opt) options
  ;     For available tag options, check out the 'interpreter' function's documentation.]
  ;  Default: [#"\".*\""]}
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
  ; (opening-match-position "<div>My content</div>" #"<div>" #"</div>")
  ; =>
  ; 0
  ;
  ; @example
  ; (opening-match-position "<div><div></div></div>" #"<div>" #"</div>")
  ; =>
  ; 0
  ;
  ; @example
  ; (opening-match-position "</div> <div></div>" #"<div>" #"</div>")
  ; =>
  ; 7
  ;
  ; @return (integer)
  ([n opening-pattern closing-pattern]
   (opening-match-position n opening-pattern closing-pattern {} {}))

  ([n opening-pattern closing-pattern tags]
   (opening-match-position n opening-pattern closing-pattern tags {}))

  ([n opening-pattern closing-pattern tags options]
   (letfn [; @param (nil) result
           ; @param (map) state
           ; {:cursor (integer)}
           ; @param (map) metafunctions
           ; {:stop (function)}
           ;
           ; @return (nil or vector)
           (f0 [_ {:keys [cursor] :as state} {:keys [stop] :as metafunctions}]
               (if (tags.utils/opening-match-found? state metafunctions)
                   (stop cursor)))]
          ; ...
          (let [tags (assoc tags :$searched-tag [opening-pattern closing-pattern])
                tags (core.prototypes/tags-prototype tags options)]
               (interpreter.engine/interpreter n f0 nil tags options)))))

(defn closing-match-position
  ; @description
  ; Returns the position of the corresponding closing tag of the first match of the given 'opening-pattern' regex pattern in the given 'n' string.
  ;
  ; @param (string) n
  ; @param (regex pattern) opening-pattern
  ; @param (regex pattern) closing-pattern
  ; @param (map)(opt) tags
  ; {:comment (vector)(opt)
  ;   [(regex pattern) pattern / opening-pattern
  ;    (regex pattern)(opt) closing-pattern
  ;    (map)(opt) options
  ;     For available tag options, check out the 'interpreter' function's documentation.]
  ;  Default: [#";.*\n"]
  ;  :quote (vector)(opt)
  ;   [(regex pattern) pattern / opening-pattern
  ;    (regex pattern)(opt) closing-pattern
  ;    (map)(opt) options
  ;     For available tag options, check out the 'interpreter' function's documentation.]
  ;  Default: [#"\".*\""]}
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
  ; (closing-match-position "<div>My content</div>" #"<div>" #"</div>")
  ; =>
  ; 15
  ;
  ; @example
  ; (closing-match-position "<div><div></div></div>" #"<div>" #"</div>")
  ; =>
  ; 16
  ;
  ; @example
  ; (closing-match-position "</div> <div></div>" #"<div>" #"</div>")
  ; =>
  ; 12
  ;
  ; @return (integer)
  ([n opening-pattern closing-pattern]
   (closing-match-position n opening-pattern closing-pattern {} {}))

  ([n opening-pattern closing-pattern tags]
   (closing-match-position n opening-pattern closing-pattern tags {}))

  ([n opening-pattern closing-pattern tags options]
   (letfn [; @param (nil) result
           ; @param (map) state
           ; {:cursor (integer)}
           ; @param (map) metafunctions
           ; {:stop (function)}
           ;
           ; @return (nil or vector)
           (f0 [_ {:keys [cursor] :as state} {:keys [stop] :as metafunctions}]
               (cond (tags.utils/closing-match-found? state metafunctions) (stop cursor)
                     (tags.utils/first-iteration?     state metafunctions) (tags.utils/init-metadata state metafunctions)))]

          ; ...
          (let [tags (assoc tags :$searched-tag [opening-pattern closing-pattern])
                tags (core.prototypes/tags-prototype tags options)]
               (interpreter.engine/interpreter n f0 nil tags options)))))
