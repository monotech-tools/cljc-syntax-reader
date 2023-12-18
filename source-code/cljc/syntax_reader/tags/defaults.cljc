
(ns syntax-reader.tags.defaults
    (:require [syntax-reader.tags.engine :as tags.engine]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn opening-brace-position
  ; @description
  ; - Returns the position of the first opening brace character in the given 'n' string.
  ; - By default, it ignores commented and quoted parts of the string.
  ;
  ; @param (string) n
  ; @param (vectors in vector)(opt)(in decreasing priority order) tags
  ; For more information, check out the documentation of the [syntax-interpreter.api/interpreter](https://mt-devtools.github.io/cljc-syntax-interpreter) function.
  ; [[(keyword) tag-name
  ;   (regex pattern) pattern / opening-pattern
  ;   (regex pattern)(opt) closing-pattern
  ;   (map)(opt) tag-options]]
  ; Default:
  ; [[:comment       #"\;"   #"\n"           {:disable-interpreter? true}]
  ;  [:meta-string   #"\^\"" #"(?<=[^\\])\"" {:disable-interpreter? true}]
  ;  [:regex-pattern #"\#\"" #"(?<=[^\\])\"" {:disable-interpreter? true}]
  ;  [:string        #"\""   #"(?<=[^\\])\"" {:disable-interpreter? true}]]
  ; @param (map)(opt) options
  ; {:endpoint (integer)(opt)
  ;   Quits searching at the given 'endpoint' position in the given 'n' string.
  ;  :offset (integer)(opt)
  ;   Starts searching at the given 'offset' position in the given 'n' string.
  ;   The returned position is an offset independent absolute value.}
  ;
  ; @usage
  ; (opening-brace-position "abc {}")
  ; =>
  ; 4
  ;
  ; @usage
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
   (tags.engine/opening-match-position n #"\{" #"\}" tags options)))

(defn closing-brace-position
  ; @description
  ; - Returns the position of the closing brace character that corresponds to the first opening brace character in the 'n' string.
  ; - By default, it ignores commented and quoted parts of the string.
  ;
  ; @param (string) n
  ; @param (vectors in vector)(opt)(in decreasing priority order) tags
  ; For more information, check out the documentation of the [syntax-interpreter.api/interpreter](https://mt-devtools.github.io/cljc-syntax-interpreter) function.
  ; [[(keyword) tag-name
  ;   (regex pattern) pattern / opening-pattern
  ;   (regex pattern)(opt) closing-pattern
  ;   (map)(opt) tag-options]]
  ; Default:
  ; [[:comment       #"\;"   #"\n"           {:disable-interpreter? true}]
  ;  [:meta-string   #"\^\"" #"(?<=[^\\])\"" {:disable-interpreter? true}]
  ;  [:regex-pattern #"\#\"" #"(?<=[^\\])\"" {:disable-interpreter? true}]
  ;  [:string        #"\""   #"(?<=[^\\])\"" {:disable-interpreter? true}]]
  ; @param (map)(opt) options
  ; {:endpoint (integer)(opt)
  ;   Quits searching at the given 'endpoint' position in the given 'n' string.
  ;  :offset (integer)(opt)
  ;   Starts searching at the given 'offset' position in the given 'n' string.
  ;   The returned position is an offset independent absolute value.}
  ;
  ; @usage
  ; (closing-brace-position "abc {}")
  ; =>
  ; 5
  ;
  ; @usage
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
   (tags.engine/closing-match-position n #"\{" #"\}" tags options)))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn opening-bracket-position
  ; @description
  ; - Returns the position of the first opening bracket character in the given 'n' string.
  ; - By default, it ignores commented and quoted parts of the string.
  ;
  ; @param (string) n
  ; @param (vectors in vector)(opt)(in decreasing priority order) tags
  ; For more information, check out the documentation of the [syntax-interpreter.api/interpreter](https://mt-devtools.github.io/cljc-syntax-interpreter) function.
  ; [[(keyword) tag-name
  ;   (regex pattern) pattern / opening-pattern
  ;   (regex pattern)(opt) closing-pattern
  ;   (map)(opt) tag-options]]
  ; Default:
  ; [[:comment       #"\;"   #"\n"           {:disable-interpreter? true}]
  ;  [:meta-string   #"\^\"" #"(?<=[^\\])\"" {:disable-interpreter? true}]
  ;  [:regex-pattern #"\#\"" #"(?<=[^\\])\"" {:disable-interpreter? true}]
  ;  [:string        #"\""   #"(?<=[^\\])\"" {:disable-interpreter? true}]]
  ; @param (map)(opt) options
  ; {:endpoint (integer)(opt)
  ;   Quits searching at the given 'endpoint' position in the given 'n' string.
  ;  :offset (integer)(opt)
  ;   Starts searching at the given 'offset' position in the given 'n' string.
  ;   The returned position is an offset independent absolute value.}
  ;
  ; @usage
  ; (opening-bracket-position "abc []")
  ; =>
  ; 4
  ;
  ; @usage
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
   (tags.engine/opening-match-position n #"\[" #"\]" tags options)))

(defn closing-bracket-position
  ; @description
  ; - Returns the position of the closing bracket character that corresponds to the first opening bracket character in the 'n' string.
  ; - By default, it ignores commented and quoted parts of the string.
  ;
  ; @param (string) n
  ; @param (vectors in vector)(opt)(in decreasing priority order) tags
  ; For more information, check out the documentation of the [syntax-interpreter.api/interpreter](https://mt-devtools.github.io/cljc-syntax-interpreter) function.
  ; [[(keyword) tag-name
  ;   (regex pattern) pattern / opening-pattern
  ;   (regex pattern)(opt) closing-pattern
  ;   (map)(opt) tag-options]]
  ; Default:
  ; [[:comment       #"\;"   #"\n"           {:disable-interpreter? true}]
  ;  [:meta-string   #"\^\"" #"(?<=[^\\])\"" {:disable-interpreter? true}]
  ;  [:regex-pattern #"\#\"" #"(?<=[^\\])\"" {:disable-interpreter? true}]
  ;  [:string        #"\""   #"(?<=[^\\])\"" {:disable-interpreter? true}]]
  ; @param (map)(opt) options
  ; {:endpoint (integer)(opt)
  ;   Quits searching at the given 'endpoint' position in the given 'n' string.
  ;  :offset (integer)(opt)
  ;   Starts searching at the given 'offset' position in the given 'n' string.
  ;   The returned position is an offset independent absolute value.}
  ;
  ; @usage
  ; (closing-bracket-position "abc []")
  ; =>
  ; 5
  ;
  ; @usage
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
   (tags.engine/closing-match-position n #"\[" #"\]" tags options)))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn opening-paren-position
  ; @description
  ; - Returns the position of the first opening parenthesis character in the given 'n' string.
  ; - By default, it ignores commented and quoted parts of the string.
  ;
  ; @param (string) n
  ; @param (vectors in vector)(opt)(in decreasing priority order) tags
  ; For more information, check out the documentation of the [syntax-interpreter.api/interpreter](https://mt-devtools.github.io/cljc-syntax-interpreter) function.
  ; [[(keyword) tag-name
  ;   (regex pattern) pattern / opening-pattern
  ;   (regex pattern)(opt) closing-pattern
  ;   (map)(opt) tag-options]]
  ; Default:
  ; [[:comment       #"\;"   #"\n"           {:disable-interpreter? true}]
  ;  [:meta-string   #"\^\"" #"(?<=[^\\])\"" {:disable-interpreter? true}]
  ;  [:regex-pattern #"\#\"" #"(?<=[^\\])\"" {:disable-interpreter? true}]
  ;  [:string        #"\""   #"(?<=[^\\])\"" {:disable-interpreter? true}]]
  ; @param (map)(opt) options
  ; {:endpoint (integer)(opt)
  ;   Quits searching at the given 'endpoint' position in the given 'n' string.
  ;  :offset (integer)(opt)
  ;   Starts searching at the given 'offset' position in the given 'n' string.
  ;   The returned position is an offset independent absolute value.}
  ;
  ; @usage
  ; (opening-paren-position "abc ()")
  ; =>
  ; 4
  ;
  ; @usage
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
   (tags.engine/opening-match-position n #"\(" #"\)" tags options)))

(defn closing-paren-position
  ; @description
  ; - Returns the position of the closing parenthesis character that corresponds to the first opening parenthesis character in the 'n' string.
  ; - By default, it ignores commented and quoted parts of the string.
  ;
  ; @param (string) n
  ; @param (vectors in vector)(opt)(in decreasing priority order) tags
  ; For more information, check out the documentation of the [syntax-interpreter.api/interpreter](https://mt-devtools.github.io/cljc-syntax-interpreter) function.
  ; [[(keyword) tag-name
  ;   (regex pattern) pattern / opening-pattern
  ;   (regex pattern)(opt) closing-pattern
  ;   (map)(opt) tag-options]]
  ; Default:
  ; [[:comment       #"\;"   #"\n"           {:disable-interpreter? true}]
  ;  [:meta-string   #"\^\"" #"(?<=[^\\])\"" {:disable-interpreter? true}]
  ;  [:regex-pattern #"\#\"" #"(?<=[^\\])\"" {:disable-interpreter? true}]
  ;  [:string        #"\""   #"(?<=[^\\])\"" {:disable-interpreter? true}]]
  ; @param (map)(opt) options
  ; {:endpoint (integer)(opt)
  ;   Quits searching at the given 'endpoint' position in the given 'n' string.
  ;  :offset (integer)(opt)
  ;   Starts searching at the given 'offset' position in the given 'n' string.
  ;   The returned position is an offset independent absolute value.}
  ;
  ; @usage
  ; (closing-paren-position "abc ()")
  ; =>
  ; 5
  ;
  ; @usage
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
   (tags.engine/closing-match-position n #"\(" #"\)" tags options)))
