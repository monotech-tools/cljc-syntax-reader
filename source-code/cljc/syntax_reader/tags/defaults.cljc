
(ns syntax-reader.tags.defaults
    (:require [syntax-reader.tags.engine :as tags.engine]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn opening-brace-position
  ; @description
  ; Returns the position of the first opening brace character in the given 'n' string.
  ;
  ; @param (string) n
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
  ; (opening-brace-position "abc {}")
  ; =>
  ; 4
  ;
  ; @example
  ; (opening-brace-position "} {}")
  ; =>
  ; 2
  ;
  ; @return (integer)
  ([n]
   (opening-brace-position n {} {}))

  ([n tags]
   (opening-brace-position n tags {}))

  ([n tags options]
   (tags.engine/opening-tag-position n #"\{" #"\}" tags options)))

(defn closing-brace-position
  ; @description
  ; Returns the position of the closing brace character that corresponds to the first opening brace character in the 'n' string.
  ;
  ; @param (string) n
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
  ; (closing-brace-position "abc {}")
  ; =>
  ; 5
  ;
  ; @example
  ; (closing-brace-position "} {}")
  ; =>
  ; 3
  ;
  ; @return (integer)
  ([n]
   (closing-brace-position n {} {}))

  ([n tags]
   (closing-brace-position n tags {}))

  ([n tags options]
   (tags.engine/closing-tag-position n #"\{" #"\}" tags options)))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn opening-bracket-position
  ; @description
  ; Returns the position of the first opening bracket character in the given 'n' string.
  ;
  ; @param (string) n
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
  ; (opening-bracket-position "abc []")
  ; =>
  ; 4
  ;
  ; @example
  ; (opening-bracket-position "] []")
  ; =>
  ; 2
  ;
  ; @return (integer)
  ([n]
   (opening-bracket-position n {} {}))

  ([n tags]
   (opening-bracket-position n tags {}))

  ([n tags options]
   (tags.engine/opening-tag-position n #"\[" #"\]" tags options)))

(defn closing-bracket-position
  ; @description
  ; Returns the position of the closing bracket character that corresponds to the first opening bracket character in the 'n' string.
  ;
  ; @param (string) n
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
  ; (closing-bracket-position "abc []")
  ; =>
  ; 5
  ;
  ; @example
  ; (closing-bracket-position "] []")
  ; =>
  ; 3
  ;
  ; @return (integer)
  ([n]
   (closing-bracket-position n {} {}))

  ([n tags]
   (closing-bracket-position n tags {}))

  ([n tags options]
   (tags.engine/closing-tag-position n #"\[" #"\]" tags options)))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn opening-paren-position
  ; @description
  ; Returns the position of the first opening parenthesis character in the given 'n' string.
  ;
  ; @param (string) n
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
  ; (opening-paren-position "abc ()")
  ; =>
  ; 4
  ;
  ; @example
  ; (opening-paren-position ") ()")
  ; =>
  ; 2
  ;
  ; @return (integer)
  ([n]
   (opening-paren-position n {} {}))

  ([n tags]
   (opening-paren-position n tags {}))

  ([n tags options]
   (tags.engine/opening-tag-position n #"\(" #"\)" tags options)))

(defn closing-paren-position
  ; @description
  ; Returns the position of the closing parenthesis character that corresponds to the first opening parenthesis character in the 'n' string.
  ;
  ; @param (string) n
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
  ; (closing-paren-position "abc ()")
  ; =>
  ; 5
  ;
  ; @example
  ; (closing-paren-position ") ()")
  ; =>
  ; 3
  ;
  ; @return (integer)
  ([n]
   (closing-paren-position n {} {}))

  ([n tags]
   (closing-paren-position n tags {}))

  ([n tags options]
   (tags.engine/closing-tag-position n #"\(" #"\)" tags options)))
