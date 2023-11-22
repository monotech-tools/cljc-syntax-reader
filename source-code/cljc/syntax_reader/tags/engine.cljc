
(ns syntax-reader.tags.engine
    (:require [syntax-reader.core.prototypes    :as core.prototypes]
              [syntax-reader.interpreter.engine :as interpreter.engine]
              [syntax-reader.tags.utils         :as tags.utils]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn opening-tag-position
  ; @description
  ; Returns the position of the first occurence of the given 'opening-tag' regex pattern in the given 'n' string.
  ;
  ; @param (string) n
  ; @param (regex pattern) opening-tag
  ; @param (regex pattern) closing-tag
  ; @param (map)(opt) tags
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
  ; (opening-tag-position "<div>My content</div>" #"<div>" #"</div>")
  ; =>
  ; 0
  ;
  ; @example
  ; (opening-tag-position "<div><div></div></div>" #"<div>" #"</div>")
  ; =>
  ; 0
  ;
  ; @example
  ; (opening-tag-position "</div> <div></div>" #"<div>" #"</div>")
  ; =>
  ; 7
  ;
  ; @return (integer)
  ([n opening-tag closing-tag]
   (opening-tag-position n opening-tag {} {}))

  ([n opening-tag closing-tag tags]
   (opening-tag-position n opening-tag tags {}))

  ([n opening-tag closing-tag tags options]
   (letfn [; @param (nil) result
           ; @param (map) state
           ; {:cursor (integer)}
           ; @param (map) metafunctions
           ; {:stop (function)}
           ;
           ; @return (nil or vector)
           (f0 [_ {:keys [cursor] :as state} {:keys [stop] :as metafunctions}]
               (if (tags.utils/opening-tag-found? state metafunctions)
                   (stop cursor)))]
          ; ...
          (let [tags (assoc tags :$searched-tag [opening-tag closing-tag])
                tags (core.prototypes/tags-prototype tags options)]
               (interpreter.engine/interpreter n f0 nil tags options)))))

(defn closing-tag-position
  ; @description
  ; Returns the position of the corresponding closing tag of the first occurence of the given 'opening-tag' regex pattern in the given 'n' string.
  ;
  ; @param (string) n
  ; @param (regex pattern) opening-tag
  ; @param (regex pattern) closing-tag
  ; @param (map)(opt) tags
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
  ;   Quits searching at the given 'endpoint' position in the given 'n' string.
  ;  :ignore-commented? (boolean)(opt)
  ;   Default: false
  ;  :ignore-escaped? (boolean)(opt)
  ;   Default: true
  ;  :ignore-quoted? (boolean)(opt)
  ;   Default: false
  ;  :offset (integer)(opt)
  ;   Starts searching at the given 'offset' position in the given 'n' string.
  ;   The returned position is an offset independent absolute value.}
  ;
  ; @example
  ; (closing-tag-position "<div>My content</div>" #"<div>" #"</div>")
  ; =>
  ; 15
  ;
  ; @example
  ; (closing-tag-position "<div><div></div></div>" #"<div>" #"</div>")
  ; =>
  ; 16
  ;
  ; @example
  ; (closing-tag-position "</div> <div></div>" #"<div>" #"</div>")
  ; =>
  ; 12
  ;
  ; @return (integer)
  ([n opening-tag closing-tag]
   (closing-tag-position n opening-tag closing-tag {} {}))

  ([n opening-tag closing-tag tags]
   (closing-tag-position n opening-tag closing-tag tags {}))

  ([n opening-tag closing-tag tags options]
   (letfn [; @param (nil) result
           ; @param (map) state
           ; {:cursor (integer)}
           ; @param (map) metafunctions
           ; {:stop (function)}
           ;
           ; @return (nil or vector)
           (f0 [_ {:keys [cursor] :as state} {:keys [stop] :as metafunctions}]
               (cond (tags.utils/closing-tag-found? state metafunctions) (stop cursor)
                     (tags.utils/first-iteration?   state metafunctions) (tags.utils/init-state state metafunctions)))]

          ; ...
          (let [tags (assoc tags :$searched-tag [opening-tag closing-tag])
                tags (core.prototypes/tags-prototype tags options)]
               (interpreter.engine/interpreter n f0 nil tags options)))))
