
(ns syntax-reader.search.defaults
    (:require [syntax-reader.search.engine :as search.engine]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn brace-starting-position
  ; @description
  ; - Returns the starting position of the first brace pair in the given 'n' string.
  ; - By default, it ignores commented and quoted parts of the string.
  ;
  ; @param (string) n
  ; @param (vectors in vector)(opt)(in decreasing priority order) tags
  ; Check out the documentation of the [syntax-interpreter.api/interpreter](https://mt-devtools.github.io/cljc-syntax-interpreter) function.
  ; [[(keyword) tag-name
  ;   (regex pattern) pattern / opening-pattern
  ;   (regex pattern)(opt) closing-pattern
  ;   (map)(opt) tag-options]]
  ; Default:
  ; [[:comment       #"\;"   #"\n"           {:accepted-children []}]
  ;  [:meta-string   #"\^\"" #"(?<=[^\\])\"" {:accepted-children []}]
  ;  [:regex-pattern #"\#\"" #"(?<=[^\\])\"" {:accepted-children []}]
  ;  [:string        #"\""   #"(?<=[^\\])\"" {:accepted-children []}]]
  ; @param (map)(opt) options
  ; Check out the documentation of the [syntax-interpreter.api/interpreter](https://mt-devtools.github.io/cljc-syntax-interpreter) function.
  ; {:endpoint (integer)(opt)
  ;   Quits searching at the given 'endpoint' position in the given 'n' string.
  ;  :offset (integer)(opt)
  ;   Starts searching at the given 'offset' position in the given 'n' string.
  ;   The returned position is an offset independent absolute value.}
  ;
  ; @usage
  ; (brace-starting-position "abc {}")
  ; =>
  ; 4
  ;
  ; @usage
  ; (brace-starting-position "} {}")
  ; =>
  ; 2
  ;
  ; @return (integer)
  ([n]
   (brace-starting-position n {} {}))

  ([n tags]
   (brace-starting-position n tags {}))

  ([n tags options]
   (search.engine/tag-starting-position n [:brace #"\{" #"\}"] tags options)))

(defn brace-opening-position
  ; @description
  ; - Returns the opening position of the first brace pair in the given 'n' string.
  ; - By default, it ignores commented and quoted parts of the string.
  ;
  ; @param (string) n
  ; @param (vectors in vector)(opt)(in decreasing priority order) tags
  ; Check out the documentation of the [syntax-interpreter.api/interpreter](https://mt-devtools.github.io/cljc-syntax-interpreter) function.
  ; [[(keyword) tag-name
  ;   (regex pattern) pattern / opening-pattern
  ;   (regex pattern)(opt) closing-pattern
  ;   (map)(opt) tag-options]]
  ; Default:
  ; [[:comment       #"\;"   #"\n"           {:accepted-children []}]
  ;  [:meta-string   #"\^\"" #"(?<=[^\\])\"" {:accepted-children []}]
  ;  [:regex-pattern #"\#\"" #"(?<=[^\\])\"" {:accepted-children []}]
  ;  [:string        #"\""   #"(?<=[^\\])\"" {:accepted-children []}]]
  ; @param (map)(opt) options
  ; Check out the documentation of the [syntax-interpreter.api/interpreter](https://mt-devtools.github.io/cljc-syntax-interpreter) function.
  ; {:endpoint (integer)(opt)
  ;   Quits searching at the given 'endpoint' position in the given 'n' string.
  ;  :offset (integer)(opt)
  ;   Starts searching at the given 'offset' position in the given 'n' string.
  ;   The returned position is an offset independent absolute value.}
  ;
  ; @usage
  ; (brace-opening-position "abc {}")
  ; =>
  ; 5
  ;
  ; @usage
  ; (brace-opening-position "} {}")
  ; =>
  ; 3
  ;
  ; @return (integer)
  ([n]
   (brace-opening-position n {} {}))

  ([n tags]
   (brace-opening-position n tags {}))

  ([n tags options]
   (search.engine/tag-opening-position n [:brace #"\{" #"\}"] tags options)))

(defn brace-closing-position
  ; @description
  ; - Returns the closing position of the first brace pair in the given 'n' string.
  ; - By default, it ignores commented and quoted parts of the string.
  ;
  ; @param (string) n
  ; @param (vectors in vector)(opt)(in decreasing priority order) tags
  ; Check out the documentation of the [syntax-interpreter.api/interpreter](https://mt-devtools.github.io/cljc-syntax-interpreter) function.
  ; [[(keyword) tag-name
  ;   (regex pattern) pattern / opening-pattern
  ;   (regex pattern)(opt) closing-pattern
  ;   (map)(opt) tag-options]]
  ; Default:
  ; [[:comment       #"\;"   #"\n"           {:accepted-children []}]
  ;  [:meta-string   #"\^\"" #"(?<=[^\\])\"" {:accepted-children []}]
  ;  [:regex-pattern #"\#\"" #"(?<=[^\\])\"" {:accepted-children []}]
  ;  [:string        #"\""   #"(?<=[^\\])\"" {:accepted-children []}]]
  ; @param (map)(opt) options
  ; Check out the documentation of the [syntax-interpreter.api/interpreter](https://mt-devtools.github.io/cljc-syntax-interpreter) function.
  ; {:endpoint (integer)(opt)
  ;   Quits searching at the given 'endpoint' position in the given 'n' string.
  ;  :offset (integer)(opt)
  ;   Starts searching at the given 'offset' position in the given 'n' string.
  ;   The returned position is an offset independent absolute value.}
  ;
  ; @usage
  ; (brace-closing-position "abc {}")
  ; =>
  ; 5
  ;
  ; @usage
  ; (brace-closing-position "} {}")
  ; =>
  ; 3
  ;
  ; @return (integer)
  ([n]
   (brace-closing-position n {} {}))

  ([n tags]
   (brace-closing-position n tags {}))

  ([n tags options]
   (search.engine/tag-closing-position n [:brace #"\{" #"\}"] tags options)))

(defn brace-ending-position
  ; @description
  ; - Returns the ending position of the first brace pair in the given 'n' string.
  ; - By default, it ignores commented and quoted parts of the string.
  ;
  ; @param (string) n
  ; @param (vectors in vector)(opt)(in decreasing priority order) tags
  ; Check out the documentation of the [syntax-interpreter.api/interpreter](https://mt-devtools.github.io/cljc-syntax-interpreter) function.
  ; [[(keyword) tag-name
  ;   (regex pattern) pattern / opening-pattern
  ;   (regex pattern)(opt) closing-pattern
  ;   (map)(opt) tag-options]]
  ; Default:
  ; [[:comment       #"\;"   #"\n"           {:accepted-children []}]
  ;  [:meta-string   #"\^\"" #"(?<=[^\\])\"" {:accepted-children []}]
  ;  [:regex-pattern #"\#\"" #"(?<=[^\\])\"" {:accepted-children []}]
  ;  [:string        #"\""   #"(?<=[^\\])\"" {:accepted-children []}]]
  ; @param (map)(opt) options
  ; Check out the documentation of the [syntax-interpreter.api/interpreter](https://mt-devtools.github.io/cljc-syntax-interpreter) function.
  ; {:endpoint (integer)(opt)
  ;   Quits searching at the given 'endpoint' position in the given 'n' string.
  ;  :offset (integer)(opt)
  ;   Starts searching at the given 'offset' position in the given 'n' string.
  ;   The returned position is an offset independent absolute value.}
  ;
  ; @usage
  ; (brace-ending-position "abc {}")
  ; =>
  ; 6
  ;
  ; @usage
  ; (brace-ending-position "} {}")
  ; =>
  ; 4
  ;
  ; @return (integer)
  ([n]
   (brace-ending-position n {} {}))

  ([n tags]
   (brace-ending-position n tags {}))

  ([n tags options]
   (search.engine/tag-ending-position n [:brace #"\{" #"\}"] tags options)))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn bracket-starting-position
  ; @description
  ; - Returns the starting position of the first bracket pair in the given 'n' string.
  ; - By default, it ignores commented and quoted parts of the string.
  ;
  ; @param (string) n
  ; @param (vectors in vector)(opt)(in decreasing priority order) tags
  ; Check out the documentation of the [syntax-interpreter.api/interpreter](https://mt-devtools.github.io/cljc-syntax-interpreter) function.
  ; [[(keyword) tag-name
  ;   (regex pattern) pattern / opening-pattern
  ;   (regex pattern)(opt) closing-pattern
  ;   (map)(opt) tag-options]]
  ; Default:
  ; [[:comment       #"\;"   #"\n"           {:accepted-children []}]
  ;  [:meta-string   #"\^\"" #"(?<=[^\\])\"" {:accepted-children []}]
  ;  [:regex-pattern #"\#\"" #"(?<=[^\\])\"" {:accepted-children []}]
  ;  [:string        #"\""   #"(?<=[^\\])\"" {:accepted-children []}]]
  ; @param (map)(opt) options
  ; Check out the documentation of the [syntax-interpreter.api/interpreter](https://mt-devtools.github.io/cljc-syntax-interpreter) function.
  ; {:endpoint (integer)(opt)
  ;   Quits searching at the given 'endpoint' position in the given 'n' string.
  ;  :offset (integer)(opt)
  ;   Starts searching at the given 'offset' position in the given 'n' string.
  ;   The returned position is an offset independent absolute value.}
  ;
  ; @usage
  ; (bracket-starting-position "abc []")
  ; =>
  ; 4
  ;
  ; @usage
  ; (bracket-starting-position "] []")
  ; =>
  ; 2
  ;
  ; @return (integer)
  ([n]
   (bracket-starting-position n {} {}))

  ([n tags]
   (bracket-starting-position n tags {}))

  ([n tags options]
   (search.engine/tag-starting-position n [:bracket #"\[" #"\]"] tags options)))

(defn bracket-opening-position
  ; @description
  ; - Returns the opening position of the first bracket pair in the given 'n' string.
  ; - By default, it ignores commented and quoted parts of the string.
  ;
  ; @param (string) n
  ; @param (vectors in vector)(opt)(in decreasing priority order) tags
  ; Check out the documentation of the [syntax-interpreter.api/interpreter](https://mt-devtools.github.io/cljc-syntax-interpreter) function.
  ; [[(keyword) tag-name
  ;   (regex pattern) pattern / opening-pattern
  ;   (regex pattern)(opt) closing-pattern
  ;   (map)(opt) tag-options]]
  ; Default:
  ; [[:comment       #"\;"   #"\n"           {:accepted-children []}]
  ;  [:meta-string   #"\^\"" #"(?<=[^\\])\"" {:accepted-children []}]
  ;  [:regex-pattern #"\#\"" #"(?<=[^\\])\"" {:accepted-children []}]
  ;  [:string        #"\""   #"(?<=[^\\])\"" {:accepted-children []}]]
  ; @param (map)(opt) options
  ; Check out the documentation of the [syntax-interpreter.api/interpreter](https://mt-devtools.github.io/cljc-syntax-interpreter) function.
  ; {:endpoint (integer)(opt)
  ;   Quits searching at the given 'endpoint' position in the given 'n' string.
  ;  :offset (integer)(opt)
  ;   Starts searching at the given 'offset' position in the given 'n' string.
  ;   The returned position is an offset independent absolute value.}
  ;
  ; @usage
  ; (bracket-opening-position "abc []")
  ; =>
  ; 5
  ;
  ; @usage
  ; (bracket-opening-position "] []")
  ; =>
  ; 3
  ;
  ; @return (integer)
  ([n]
   (bracket-opening-position n {} {}))

  ([n tags]
   (bracket-opening-position n tags {}))

  ([n tags options]
   (search.engine/tag-opening-position n [:bracket #"\[" #"\]"] tags options)))

(defn bracket-closing-position
  ; @description
  ; - Returns the closing position of the first bracket pair in the given 'n' string.
  ; - By default, it ignores commented and quoted parts of the string.
  ;
  ; @param (string) n
  ; @param (vectors in vector)(opt)(in decreasing priority order) tags
  ; Check out the documentation of the [syntax-interpreter.api/interpreter](https://mt-devtools.github.io/cljc-syntax-interpreter) function.
  ; [[(keyword) tag-name
  ;   (regex pattern) pattern / opening-pattern
  ;   (regex pattern)(opt) closing-pattern
  ;   (map)(opt) tag-options]]
  ; Default:
  ; [[:comment       #"\;"   #"\n"           {:accepted-children []}]
  ;  [:meta-string   #"\^\"" #"(?<=[^\\])\"" {:accepted-children []}]
  ;  [:regex-pattern #"\#\"" #"(?<=[^\\])\"" {:accepted-children []}]
  ;  [:string        #"\""   #"(?<=[^\\])\"" {:accepted-children []}]]
  ; @param (map)(opt) options
  ; Check out the documentation of the [syntax-interpreter.api/interpreter](https://mt-devtools.github.io/cljc-syntax-interpreter) function.
  ; {:endpoint (integer)(opt)
  ;   Quits searching at the given 'endpoint' position in the given 'n' string.
  ;  :offset (integer)(opt)
  ;   Starts searching at the given 'offset' position in the given 'n' string.
  ;   The returned position is an offset independent absolute value.}
  ;
  ; @usage
  ; (bracket-closing-position "abc []")
  ; =>
  ; 5
  ;
  ; @usage
  ; (bracket-closing-position "] []")
  ; =>
  ; 3
  ;
  ; @return (integer)
  ([n]
   (bracket-closing-position n {} {}))

  ([n tags]
   (bracket-closing-position n tags {}))

  ([n tags options]
   (search.engine/tag-closing-position n [:bracket #"\[" #"\]"] tags options)))

(defn bracket-ending-position
  ; @description
  ; - Returns the ending position of the first bracket pair in the given 'n' string.
  ; - By default, it ignores commented and quoted parts of the string.
  ;
  ; @param (string) n
  ; @param (vectors in vector)(opt)(in decreasing priority order) tags
  ; Check out the documentation of the [syntax-interpreter.api/interpreter](https://mt-devtools.github.io/cljc-syntax-interpreter) function.
  ; [[(keyword) tag-name
  ;   (regex pattern) pattern / opening-pattern
  ;   (regex pattern)(opt) closing-pattern
  ;   (map)(opt) tag-options]]
  ; Default:
  ; [[:comment       #"\;"   #"\n"           {:accepted-children []}]
  ;  [:meta-string   #"\^\"" #"(?<=[^\\])\"" {:accepted-children []}]
  ;  [:regex-pattern #"\#\"" #"(?<=[^\\])\"" {:accepted-children []}]
  ;  [:string        #"\""   #"(?<=[^\\])\"" {:accepted-children []}]]
  ; @param (map)(opt) options
  ; Check out the documentation of the [syntax-interpreter.api/interpreter](https://mt-devtools.github.io/cljc-syntax-interpreter) function.
  ; {:endpoint (integer)(opt)
  ;   Quits searching at the given 'endpoint' position in the given 'n' string.
  ;  :offset (integer)(opt)
  ;   Starts searching at the given 'offset' position in the given 'n' string.
  ;   The returned position is an offset independent absolute value.}
  ;
  ; @usage
  ; (bracket-ending-position "abc []")
  ; =>
  ; 6
  ;
  ; @usage
  ; (bracket-ending-position "] []")
  ; =>
  ; 4
  ;
  ; @return (integer)
  ([n]
   (bracket-ending-position n {} {}))

  ([n tags]
   (bracket-ending-position n tags {}))

  ([n tags options]
   (search.engine/tag-ending-position n [:bracket #"\[" #"\]"] tags options)))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn paren-starting-position
  ; @description
  ; - Returns the starting position of the first parenthesis pair in the given 'n' string.
  ; - By default, it ignores commented and quoted parts of the string.
  ;
  ; @param (string) n
  ; @param (vectors in vector)(opt)(in decreasing priority order) tags
  ; Check out the documentation of the [syntax-interpreter.api/interpreter](https://mt-devtools.github.io/cljc-syntax-interpreter) function.
  ; [[(keyword) tag-name
  ;   (regex pattern) pattern / opening-pattern
  ;   (regex pattern)(opt) closing-pattern
  ;   (map)(opt) tag-options]]
  ; Default:
  ; [[:comment       #"\;"   #"\n"           {:accepted-children []}]
  ;  [:meta-string   #"\^\"" #"(?<=[^\\])\"" {:accepted-children []}]
  ;  [:regex-pattern #"\#\"" #"(?<=[^\\])\"" {:accepted-children []}]
  ;  [:string        #"\""   #"(?<=[^\\])\"" {:accepted-children []}]]
  ; @param (map)(opt) options
  ; Check out the documentation of the [syntax-interpreter.api/interpreter](https://mt-devtools.github.io/cljc-syntax-interpreter) function.
  ; {:endpoint (integer)(opt)
  ;   Quits searching at the given 'endpoint' position in the given 'n' string.
  ;  :offset (integer)(opt)
  ;   Starts searching at the given 'offset' position in the given 'n' string.
  ;   The returned position is an offset independent absolute value.}
  ;
  ; @usage
  ; (paren-starting-position "abc ()")
  ; =>
  ; 4
  ;
  ; @usage
  ; (paren-starting-position ") ()")
  ; =>
  ; 2
  ;
  ; @return (integer)
  ([n]
   (paren-starting-position n {} {}))

  ([n tags]
   (paren-starting-position n tags {}))

  ([n tags options]
   (search.engine/tag-starting-position n [:paren #"\(" #"\)"] tags options)))

(defn paren-opening-position
  ; @description
  ; - Returns the opening position of the first parenthesis pair in the given 'n' string.
  ; - By default, it ignores commented and quoted parts of the string.
  ;
  ; @param (string) n
  ; @param (vectors in vector)(opt)(in decreasing priority order) tags
  ; Check out the documentation of the [syntax-interpreter.api/interpreter](https://mt-devtools.github.io/cljc-syntax-interpreter) function.
  ; [[(keyword) tag-name
  ;   (regex pattern) pattern / opening-pattern
  ;   (regex pattern)(opt) closing-pattern
  ;   (map)(opt) tag-options]]
  ; Default:
  ; [[:comment       #"\;"   #"\n"           {:accepted-children []}]
  ;  [:meta-string   #"\^\"" #"(?<=[^\\])\"" {:accepted-children []}]
  ;  [:regex-pattern #"\#\"" #"(?<=[^\\])\"" {:accepted-children []}]
  ;  [:string        #"\""   #"(?<=[^\\])\"" {:accepted-children []}]]
  ; @param (map)(opt) options
  ; Check out the documentation of the [syntax-interpreter.api/interpreter](https://mt-devtools.github.io/cljc-syntax-interpreter) function.
  ; {:endpoint (integer)(opt)
  ;   Quits searching at the given 'endpoint' position in the given 'n' string.
  ;  :offset (integer)(opt)
  ;   Starts searching at the given 'offset' position in the given 'n' string.
  ;   The returned position is an offset independent absolute value.}
  ;
  ; @usage
  ; (paren-opening-position "abc ()")
  ; =>
  ; 5
  ;
  ; @usage
  ; (paren-opening-position ") ()")
  ; =>
  ; 3
  ;
  ; @return (integer)
  ([n]
   (paren-opening-position n {} {}))

  ([n tags]
   (paren-opening-position n tags {}))

  ([n tags options]
   (search.engine/tag-opening-position n [:paren #"\(" #"\)"] tags options)))

(defn paren-closing-position
  ; @description
  ; - Returns the closing position of the first parenthesis pair in the given 'n' string.
  ; - By default, it ignores commented and quoted parts of the string.
  ;
  ; @param (string) n
  ; @param (vectors in vector)(opt)(in decreasing priority order) tags
  ; Check out the documentation of the [syntax-interpreter.api/interpreter](https://mt-devtools.github.io/cljc-syntax-interpreter) function.
  ; [[(keyword) tag-name
  ;   (regex pattern) pattern / opening-pattern
  ;   (regex pattern)(opt) closing-pattern
  ;   (map)(opt) tag-options]]
  ; Default:
  ; [[:comment       #"\;"   #"\n"           {:accepted-children []}]
  ;  [:meta-string   #"\^\"" #"(?<=[^\\])\"" {:accepted-children []}]
  ;  [:regex-pattern #"\#\"" #"(?<=[^\\])\"" {:accepted-children []}]
  ;  [:string        #"\""   #"(?<=[^\\])\"" {:accepted-children []}]]
  ; @param (map)(opt) options
  ; Check out the documentation of the [syntax-interpreter.api/interpreter](https://mt-devtools.github.io/cljc-syntax-interpreter) function.
  ; {:endpoint (integer)(opt)
  ;   Quits searching at the given 'endpoint' position in the given 'n' string.
  ;  :offset (integer)(opt)
  ;   Starts searching at the given 'offset' position in the given 'n' string.
  ;   The returned position is an offset independent absolute value.}
  ;
  ; @usage
  ; (paren-closing-position "abc ()")
  ; =>
  ; 5
  ;
  ; @usage
  ; (paren-closing-position ") ()")
  ; =>
  ; 3
  ;
  ; @return (integer)
  ([n]
   (paren-closing-position n {} {}))

  ([n tags]
   (paren-closing-position n tags {}))

  ([n tags options]
   (search.engine/tag-closing-position n [:paren #"\(" #"\)"] tags options)))

(defn paren-ending-position
  ; @description
  ; - Returns the ending position of the first parenthesis pair in the given 'n' string.
  ; - By default, it ignores commented and quoted parts of the string.
  ;
  ; @param (string) n
  ; @param (vectors in vector)(opt)(in decreasing priority order) tags
  ; Check out the documentation of the [syntax-interpreter.api/interpreter](https://mt-devtools.github.io/cljc-syntax-interpreter) function.
  ; [[(keyword) tag-name
  ;   (regex pattern) pattern / opening-pattern
  ;   (regex pattern)(opt) closing-pattern
  ;   (map)(opt) tag-options]]
  ; Default:
  ; [[:comment       #"\;"   #"\n"           {:accepted-children []}]
  ;  [:meta-string   #"\^\"" #"(?<=[^\\])\"" {:accepted-children []}]
  ;  [:regex-pattern #"\#\"" #"(?<=[^\\])\"" {:accepted-children []}]
  ;  [:string        #"\""   #"(?<=[^\\])\"" {:accepted-children []}]]
  ; @param (map)(opt) options
  ; Check out the documentation of the [syntax-interpreter.api/interpreter](https://mt-devtools.github.io/cljc-syntax-interpreter) function.
  ; {:endpoint (integer)(opt)
  ;   Quits searching at the given 'endpoint' position in the given 'n' string.
  ;  :offset (integer)(opt)
  ;   Starts searching at the given 'offset' position in the given 'n' string.
  ;   The returned position is an offset independent absolute value.}
  ;
  ; @usage
  ; (paren-ending-position "abc ()")
  ; =>
  ; 6
  ;
  ; @usage
  ; (paren-ending-position ") ()")
  ; =>
  ; 4
  ;
  ; @return (integer)
  ([n]
   (paren-ending-position n {} {}))

  ([n tags]
   (paren-ending-position n tags {}))

  ([n tags options]
   (search.engine/tag-ending-position n [:paren #"\(" #"\)"] tags options)))
