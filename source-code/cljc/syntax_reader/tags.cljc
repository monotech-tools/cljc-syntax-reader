
(ns syntax-reader.tags
    (:require [seqable.api               :as seqable]
              [string.api                :as string]
              [syntax-reader.check       :as check]
              [syntax-reader.interpreter :as interpreter]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn tag-first-position
  ; @description
  ; Returns the position of the first occurence of the 'tag' string in the 'n' string.
  ;
  ; @param (string) n
  ; @param (string) tag
  ; @param (map)(opt) tags
  ; {:comment-close-tag (string)(opt)
  ;   Default: "\n"
  ;  :comment-open-tag (string)(opt)
  ;   Default ";"
  ;  :quote-close-tag (string)(opt)
  ;  :quote-open-tag (string)(opt)
  ;   Default: "\""}
  ; @param (map)(opt) options
  ; {:endpoint (integer)(opt)
  ;   Stops the searching at the endpoint position in the given 'n' string.
  ;  :ignore-commented? (boolean)(opt)
  ;   Default: true
  ;  :ignore-escaped? (boolean)(opt)
  ;   Default: true
  ;  :ignore-quoted? (boolean)(opt)
  ;   Default: true
  ;  :offset (integer)(not available)
  ;   It would be great to use an offset value where the search could start in the given 'n' string,
  ;   but in order to make accurate grey-zone map (commented / quoted parts) the search must start at the beginning of the string.}
  ;
  ; @example
  ; (tag-first-position "<div>My content</div>" "<div>")
  ; =>
  ; 0
  ;
  ; @example
  ; (tag-first-position "<div><div></div></div>" "<div>")
  ; =>
  ; 0
  ;
  ; @example
  ; (tag-first-position "</div> <div></div>" "<div>")
  ; =>
  ; 7
  ;
  ; @return (integer)
  ([n tag]
   (tag-first-position n tag {} {}))

  ([n tag tags options]))

   ;(let [offset        (seqable/normalize-cursor n offset)
    ;     observed-part (string/keep-range              n offset)]))
        ;(if (or ignore-commented? ignore-quoted?)))))
            ;(let [grey-zones (interpreter/grey-zones n)])))))
    ;        (if-let [observed-tag-pos (string/first-dex-of observed-part tag)]
    ;                (let [observed-tag-pos (+ offset observed-tag-pos)]
    ;                     (if ignore-commented? (if (check/position-commented? n observed-tag-pos comment-open-tag comment-close-tag)
    ;                                              (tag-first-position n tag (assoc options :offset (+ observed-tag-pos (count tag))))
    ;                                              (-> observed-tag-pos)
    ;                                          (-> observed-tag-pos)]))

(defn tag-count
  ; @description
  ; - Returns the found occurence count of the 'tag' string in the 'n' string.
  ; - If the 'offset' parameter is passed, the search starts from the offset position.
  ;
  ; @param (string) n
  ; @param (string) tag
  ; @param (map)(opt) options
  ; {:comment-close-tag (string)(opt)
  ;   Default: "\n"
  ;  :comment-open-tag (string)(opt)
  ;   Default ";"
  ;  :ignore-commented? (boolean)(opt)
  ;   Default: false
  ;  :ignore-quoted? (boolean)(opt)
  ;   Default: false
  ;  :offset (integer)(opt)
  ;   Default: 0}
  ;
  ; @example
  ; (tag-count "<div><div></div></div>" "<div>")
  ; =>
  ; 2
  ;
  ; @return (integer)
  ([n tag]
   (tag-count n tag {}))

  ([n tag {:keys [offset] :or {offset 0} :as options}]
   (letfn [(f [cursor found-tag-count]
              (if (seqable/cursor-in-bounds? n cursor)
                  (if-let [first-tag-pos (tag-first-position n tag (assoc options :offset cursor))]
                          (f (+ first-tag-pos (count tag)) (inc found-tag-count))
                          (-> found-tag-count))
                  (-> found-tag-count)))]
          ; ...
          (f offset 0))))

(defn tags-balanced?
  ; @description
  ; - Returns TRUE if the given 'open-tag' and 'close-tag' pairs are balanced in their quantity in the given 'n' string.
  ; - If the 'offset' parameter is passed, the search starts from the offset position.
  ;
  ; @param (string) n
  ; @param (string) open-tag
  ; @param (string) close-tag
  ; @param (map)(opt) options
  ; {:comment-close-tag (string)(opt)
  ;   Default: "\n"
  ;  :comment-open-tag (string)(opt)
  ;   Default ";"
  ;  :ignore-commented? (boolean)(opt)
  ;   Default: false
  ;  :ignore-quoted? (boolean)(opt)
  ;   Default: false
  ;  :offset (integer)(opt)
  ;   Default: 0}
  ;
  ; @example
  ; (tags-balanced? "<div><div></div>" "<div>" "</div>")
  ; =>
  ; false
  ;
  ; @example
  ; (tags-balanced? "<div><div></div></div>" "<div>" "</div>")
  ; =>
  ; true
  ;
  ; @return (boolean)
  ([n open-tag close-tag]
   (tags-balanced? n open-tag close-tag {}))

  ([n open-tag close-tag options]
   (= (tag-count n open-tag  options)
      (tag-count n close-tag options))))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn tag-opening-position
  ; @description
  ; - Returns the position of the first 'open-tag' string in the 'n' string.
  ; - If the 'offset' parameter is passed, the search starts from the offset position.
  ; - The returned position is an absolute value and it is independent from the offset.
  ;
  ; @param (string) n
  ; @param (string) open-tag
  ; @param (map)(opt) options
  ; {:comment-close-tag (string)(opt)
  ;   Default: "\n"
  ;  :comment-open-tag (string)(opt)
  ;   Default ";"
  ;  :ignore-commented? (boolean)(opt)
  ;   Default: false
  ;  :ignore-quoted? (boolean)(opt)
  ;   Default: false
  ;  :offset (integer)(opt)
  ;   Default: 0}
  ;
  ; @example
  ; (tag-opening-position "<div>My content</div>" "<div>")
  ; =>
  ; 0
  ;
  ; @example
  ; (tag-opening-position "<div><div></div></div>" "<div>")
  ; =>
  ; 0
  ;
  ; @example
  ; (tag-opening-position "</div> <div></div>" "<div>")
  ; =>
  ; 7
  ;
  ; @return (integer)
  ([n open-tag]
   (tag-opening-position n open-tag {}))

  ([n open-tag options]
   (tag-first-position n open-tag options)))

(defn tag-closing-position
  ; @description
  ; - Returns the position of the close pair of the first occurence of the 'open-tag' string in the 'n' string.
  ; - If the 'offset' parameter is passed, the search starts from the offset position.
  ; - The returned position is an absolute value and it is independent from the offset.
  ;
  ; @param (string) n
  ; @param (string) open-tag
  ; @param (string) close-tag
  ; @param (map)(opt) options
  ; {:comment-close-tag (string)(opt)
  ;   Default: "\n"
  ;  :comment-open-tag (string)(opt)
  ;   Default ";"
  ;  :ignore-commented? (boolean)(opt)
  ;   Default: false
  ;  :ignore-quoted? (boolean)(opt)
  ;   Default: false
  ;  :offset (integer)(opt)
  ;   Default: 0}
  ;
  ; @example
  ; (tag-closing-position "<div>My content</div>" "<div>" "</div>")
  ; =>
  ; 15
  ;
  ; @example
  ; (tag-closing-position "<div><div></div></div>" "<div>" "</div>")
  ; =>
  ; 16
  ;
  ; @example
  ; (tag-closing-position "</div> <div></div>" "<div>" "</div>")
  ; =>
  ; 12 <- DEPRECTATED BEHAVIOUR
  ; 0
  ;
  ; @return (integer)
  ([n open-tag close-tag]
   (tag-closing-position n open-tag close-tag {}))

  ([n open-tag close-tag {:keys [offset] :or {offset 0} :as options}]
   (letfn [(f [cursor]
              (if (seqable/cursor-in-bounds? n cursor)
                  (if-let [observed-close-pos (tag-first-position n close-tag (assoc options :offset cursor))]
                          (let [observed-part (string/keep-range n 0 (+ observed-close-pos (count close-tag)))]
                               (if (tags-balanced? observed-part open-tag close-tag options)
                                   (-> observed-close-pos)
                                   (f (+ observed-close-pos (count close-tag))))))))]
          ; ...
          (f offset))))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn brace-opening-position
  ; @description
  ; - Returns the position of the first opening brace character in the 'n' string.
  ; - If the 'offset' parameter is passed, the search starts from the offset position.
  ; - The returned position is an absolute value and it is independent from the offset.
  ;
  ; @param (string) n
  ; @param (map)(opt) options
  ; {:comment-close-tag (string)(opt)
  ;   Default: "\n"
  ;  :comment-open-tag (string)(opt)
  ;   Default ";"
  ;  :ignore-commented? (boolean)(opt)
  ;   Default: false
  ;  :ignore-quoted? (boolean)(opt)
  ;   Default: false
  ;  :offset (integer)(opt)
  ;   Default: 0}
  ;
  ; @example
  ; (brace-opening-position "{:a 0}")
  ; =>
  ; 0
  ;
  ; @example
  ; (brace-opening-position "([] {:a {:b 0}})")
  ; =>
  ; 4
  ;
  ; @example
  ; (brace-opening-position "} {}")
  ; =>
  ; 2
  ;
  ; @return (integer)
  ([n]
   (brace-opening-position n {}))

  ([n options]
   (tag-opening-position n "{" options)))

(defn brace-closing-position
  ; @description
  ; - Returns the position of the closing brace that corresponds to the first opening brace character in the 'n' string.
  ; - If the 'offset' parameter is passed, the search starts from the offset position.
  ; - The returned position is an absolute value and does not depend on the offset.
  ;
  ; @param (string) n
  ; @param (map)(opt) options
  ; {:comment-close-tag (string)(opt)
  ;   Default: "\n"
  ;  :comment-open-tag (string)(opt)
  ;   Default ";"
  ;  :ignore-commented? (boolean)(opt)
  ;   Default: false
  ;  :ignore-quoted? (boolean)(opt)
  ;   Default: false
  ;  :offset (integer)(opt)
  ;   Default: 0}
  ;
  ; @example
  ; (brace-closing-position "{:a 0}")
  ; =>
  ; 5
  ;
  ; @example
  ; (brace-closing-position "([] {:a {:b 0}})")
  ; =>
  ; 14
  ;
  ; @example
  ; (brace-closing-position "} {}")
  ; =>
  ; 3
  ;
  ; @return (integer)
  ([n]
   (brace-closing-position n {}))

  ([n options]
   (tag-closing-position n "{" "}" options)))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn bracket-opening-position
  ; @description
  ; - Returns the position of the first opening bracket character in the 'n' string.
  ; - If the 'offset' parameter is passed, the search starts from the offset position.
  ; - The returned position is an absolute value and it is independent from the offset.
  ;
  ; @param (string) n
  ; @param (map)(opt) options
  ; {:comment-close-tag (string)(opt)
  ;   Default: "\n"
  ;  :comment-open-tag (string)(opt)
  ;   Default ";"
  ;  :ignore-commented? (boolean)(opt)
  ;   Default: false
  ;  :ignore-quoted? (boolean)(opt)
  ;   Default: false
  ;  :offset (integer)(opt)
  ;   Default: 0}
  ;
  ; @example
  ; (bracket-opening-position "[1 2]")
  ; =>
  ; 0
  ;
  ; @example
  ; (bracket-opening-position "({} [[0 1]])")
  ; =>
  ; 4
  ;
  ; @example
  ; (bracket-opening-position "] []")
  ; =>
  ; 2
  ;
  ; @return (integer)
  ([n]
   (bracket-opening-position n {}))

  ([n options]
   (tag-opening-position n "[" options)))

(defn bracket-closing-position
  ; @description
  ; - Returns the position of the closing bracket that corresponds to the first opening bracket character in the 'n' string.
  ; - If the 'offset' parameter is passed, the search starts from the offset position.
  ; - The returned position is an absolute value and does not depend on the offset.
  ;
  ; @param (string) n
  ; @param (map)(opt) options
  ; {:comment-close-tag (string)(opt)
  ;   Default: "\n"
  ;  :comment-open-tag (string)(opt)
  ;   Default ";"
  ;  :ignore-commented? (boolean)(opt)
  ;   Default: false
  ;  :ignore-quoted? (boolean)(opt)
  ;   Default: false
  ;  :offset (integer)(opt)
  ;   Default: 0}
  ;
  ; @example
  ; (bracket-closing-position "[1 2]")
  ; =>
  ; 4
  ;
  ; @example
  ; (bracket-closing-position "({} [[0 1]])")
  ; =>
  ; 10
  ;
  ; @example
  ; (bracket-closing-position "] []")
  ; =>
  ; 3
  ;
  ; @return (integer)
  ([n]
   (bracket-closing-position n {}))

  ([n options]
   (tag-closing-position n "[" "]" options)))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn paren-opening-position
  ; @description
  ; - Returns the position of the first opening parenthesis character in the 'n' string.
  ; - If the 'offset' parameter is passed, the search starts from the offset position.
  ; - The returned position is an absolute value and it is independent from the offset.
  ;
  ; @param (string) n
  ; @param (map)(opt) options
  ; {:comment-close-tag (string)(opt)
  ;   Default: "\n"
  ;  :comment-open-tag (string)(opt)
  ;   Default ";"
  ;  :ignore-commented? (boolean)(opt)
  ;   Default: false
  ;  :ignore-quoted? (boolean)(opt)
  ;   Default: false
  ;  :offset (integer)(opt)
  ;   Default: 0}
  ;
  ; @example
  ; (paren-opening-position "(+ 1 2)")
  ; =>
  ; 0
  ;
  ; @example
  ; (paren-opening-position "[{} (+ 1 (inc 2) (inc 3))]")
  ; =>
  ; 4
  ;
  ; @return (integer)
  ([n]
   (paren-opening-position n {}))

  ([n options]
   ; The 'bithandshake/clj-docs-generator' library would throw an error of unbalanced parens
   ; without this little fella' -> :)
   (tag-opening-position n "(" options)))

(defn paren-closing-position
  ; @description
  ; - Returns the position of the closing parenthesis that corresponds to the first opening parenthesis character in the 'n' string.
  ; - If the 'offset' parameter is passed, the search starts from the offset position.
  ; - The returned position is an absolute value and does not depend on the offset.
  ;
  ; @param (string) n
  ; @param (map)(opt) options
  ; {:comment-close-tag (string)(opt)
  ;   Default: "\n"
  ;  :comment-open-tag (string)(opt)
  ;   Default ";"
  ;  :ignore-commented? (boolean)(opt)
  ;   Default: false
  ;  :ignore-quoted? (boolean)(opt)
  ;   Default: false
  ;  :offset (integer)(opt)
  ;   Default: 0}
  ;
  ; @example
  ; (paren-closing-position "(+ 1 2)")
  ; =>
  ; 6
  ;
  ; @example
  ; (paren-closing-position "[{} (+ 1 (inc 2) (inc 3))]")
  ; =>
  ; 24
  ;
  ; @return (integer)
  ([n]
   (paren-closing-position n {}))

  ([n options]
   (tag-closing-position n "(" ")" options)))
