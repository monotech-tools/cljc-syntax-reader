
(ns syntax-reader.default-patterns)

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

; @important
; - In the following type patterns the positive lookarounds contain the double quote character as an optional
;   preceding or following character to allow strings to precede or follow other types without whitespaces between them.
;   To prevent other types from being accindentally misread as a string, the string pattern has higher priority than other type patterns.
; - Comment, regex and string patterns disable the interpreter while opened to prevent reading commented / quoted matches.
; - Providing lookbehind, lookahead and match length limits helps decrease the processing time.
;
; @description
; - Strings can precede or follow any other type without whitespaces between them.
;   E.g., "string":keyword"string", "string"false"string", etc.
; - Dereference operator shorthand ("@") can follow any other type without whitespaces between them.
;   E.g., "string"@(atom nil)
; - Data structures can precede or follow any other type without whitespaces between them.
;   E.g., "string"{}()[]true
; - Symbols can contain allowed special characters: + - * / = < > ! ? _ % & . ~ ^ # : '
;   Except the first character that cannot be a hashtag (#), a colon (:), or a single quote (').
; - Keywords are the same as symbols except their first character is a colon.
;   E.g., :keyword
; - Unresolved symbols are the same as symbols except their first character is a single quote (').
;   E.g., 'unresolved-symbol
; - Vars are the same as symbols except their first characters are a hashtag and a single quote (#').
;   E.g., #'var
;
; @constant (map)
; {:my-tag (vector)
;   [(regex-pattern) pattern / opening-pattern
;    (regex-pattern)(opt) closing-pattern
;    (map)(opt) options]}
(def CLJ-PATTERNS
     {:boolean    [#"(?<=[\n\r\s\t\[\]\(\)\{\}\"])true|false(?=[\n\r\s\t\[\]\(\)\{\}\"\@])"                                                                                     {:pattern-limits {:lookbehind 1 :match 5 :lookahead 1}}]
      :keyword    [#"(?<=[\n\r\s\t\[\]\(\)\{\}\"])\:[a-zA-Z\d\+\-\*\/\=\<\>\!\?\_\%\&\.\~\^\#\'\:]{1,}(?=[\n\r\s\t\[\]\(\)\{\}\"\@])"                                           {:pattern-limits {:lookbehind 1          :lookahead 1}}]
      :symbol     [#"(?<=[\n\r\s\t\[\]\(\)\{\}\"])[a-zA-Z\d\+\-\*\/\=\<\>\!\?\_\%\&\.\~\^][a-zA-Z\d\+\-\*\/\=\<\>\!\?\_\%\&\.\~\^\#\'\:]{0,}(?=[\n\r\s\t\[\]\(\)\{\}\"\@])"     {:pattern-limits {:lookbehind 1          :lookahead 1}}]
      :unresolved [#"(?<=[\n\r\s\t\[\]\(\)\{\}\"])\'[a-zA-Z\d\+\-\*\/\=\<\>\!\?\_\%\&\.\~\^][a-zA-Z\d\+\-\*\/\=\<\>\!\?\_\%\&\.\~\^\#\'\:]{0,}(?=[\n\r\s\t\[\]\(\)\{\}\"\@])"   {:pattern-limits {:lookbehind 1          :lookahead 1}}]
      :var        [#"(?<=[\n\r\s\t\[\]\(\)\{\}\"])\#\'[a-zA-Z\d\+\-\*\/\=\<\>\!\?\_\%\&\.\~\^][a-zA-Z\d\+\-\*\/\=\<\>\!\?\_\%\&\.\~\^\#\'\:]{0,}(?=[\n\r\s\t\[\]\(\)\{\}\"\@])" {:pattern-limits {:lookbehind 1          :lookahead 1}}]
      :list       [#"\(" #"\)" {:pattern-limits {:lookbehind 0 :match 1 :lookahead 0}}]
      :map        [#"\{" #"\}" {:pattern-limits {:lookbehind 0 :match 1 :lookahead 0}}]
      :vector     [#"\[" #"\]" {:pattern-limits {:lookbehind 0 :match 1 :lookahead 0}}]
      :comment    [#";"  #"\n" {:priority :high :disable-interpreter? true :pattern-limits {:lookbehind 0 :match 1 :lookahead 0}}]
      :regex      [#"\"" #"\"" {:priority :high :disable-interpreter? true :pattern-limits {:lookbehind 0 :match 1 :lookahead 0}}]
      :string     [#"\"" #"\"" {:priority :high :disable-interpreter? true :pattern-limits {:lookbehind 0 :match 1 :lookahead 0}}]})

; @important
; Providing lookbehind, lookahead and match length limits helps decrease the processing time.
;
; @description
; - Class names can contain letters, digits, hyphens and underscores.
;   Except the first character that cannot be a digit or a hyphen.
; - Identifiers can contain letters, digits, hyphens and underscores.
;   Except the first character that cannot be a digit or a hyphen.
; - Tag names can contain only letters.
;
; @constant (map)
; {:my-tag (vector)
;   [(regex-pattern) pattern / opening-pattern
;    (regex-pattern)(opt) closing-pattern
;    (map)(opt) options]}
(def CSS-PATTERNS
     {:class [#"(?<=[\n\r\s\t\}\]\)\*\~\>\+a-zA-Z\d\_\-])\.[a-zA-Z\d\_][a-zA-Z\d\_\-]{0,}(?<=[\n\r\s\t\{\[\*\~\>\:\.\#])" {:pattern-limits {:lookbehind 1 :lookahead 1}}]
      :id    [#"(?<=[\n\r\s\t\}\]\)\*\~\>\+a-zA-Z\d\_\-])\#[a-zA-Z\d\_][a-zA-Z\d\_\-]{0,}(?<=[\n\r\s\t\{\[\*\~\>\:\.\#])" {:pattern-limits {:lookbehind 1 :lookahead 1}}]
      :name  [#"(?<=[\n\r\s\t\}\]\)\*\~\>\+a-zA-Z\d\_\-])[a-zA-Z]{1,}(?<=[\n\r\s\t\{\[\*\~\>\:\.\#])"                     {:pattern-limits {:lookbehind 1 :lookahead 1}}]})
