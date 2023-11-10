
(ns syntax.interpreter
    (:require [string.api   :as string]
              [syntax.check :as check]
              [vector.api   :as vector]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn tag-positions
  ; @description
  ; Returns a map that contains all the positions of the given tags found in the given 'n' string.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; {:my-tag-name (string)}
  ; @param (map)(opt) options
  ; {:ignore-escaped? (boolean)(opt)
  ;   Default: true}
  ;
  ; @usage
  ; (tag-positions "(defn my-function [])" {:open-paren "(" :close-paren ")"})
  ;
  ; @example
  ; (tag-positions "(defn my-function [])" {:open-paren "(" :close-paren ")"})
  ; =>
  ; {:open-paren [0] :close-paren [20]}
  ;
  ; @example
  ; (tag-positions "(defn my-function [])" {:open-paren "(" :close-paren ")" :open-bracket "[" :close-bracket "]"})
  ; =>
  ; {:open-paren    [0]
  ;  :open-bracket  [18]
  ;  :close-bracket [19]
  ;  :close-paren   [20]}
  ;
  ; @return (map)
  ([n tags]
   (tag-positions n tags {}))

  ([n tags {:keys [ignore-escaped?] :or {ignore-escaped? true}}]
   (letfn [; If the given tag starts at the actual cursor it updates the state and returns it, otherwise it returns NIL.
           (f0 [state cursor [tag-name tag]]
               (cond ; Returns NIL if the 'ignore-escaped?' option is TRUE and an escape character preceeds the cursor.
                     (and ignore-escaped? (check/position-escaped? n cursor))
                     (-> nil)
                     ; Returns the updated state if the given 'tag' string starts at the given 'cursor' value in the 'n' string.
                     (string/starts-at? n tag cursor)
                     (update state tag-name vector/conj-item cursor)))

           ; If the 'f0' function found something and returned an updated state it returns the updated state,
           ; otherwise iz returns the original / given state.
           (f1 [state cursor] (or (some #(f0 state cursor %) tags) state))]

          ; Iterates over the given 'n' string and applies the 'f0' function at every cursor position.
          (string/walk n {} f1))))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn grey-zones
  ; @description
  ; - Returns the ranges (zone boundaries) in the given 'n' string that are commented / quoted.
  ; - If the ':comment-close-tag' / ':quote-close-tag' is not passed it uses the comment / quote opening tag as a closing tag.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; {:comment-close-tag (string)(opt)
  ;  :comment-open-tag (string)
  ;  :quote-close-tag (string)(opt)
  ;  :quote-open-tag (string)}
  ; @param (map) options
  ; {:ignore-escaped? (boolean)(opt)
  ;   Default: true}
  ;
  ; @usage
  ; (grey-zones "My string" {...})
  ;
  ; @usage
  ; (grey-zones "My string ; My comment\n \"My quote\"" {:comment-open-tag ";" :comment-close-tag "\n" :quote-open-tag "\""})
  ;
  ; @example
  ; (grey-zones "My string ; My comment\n \"My quote\"" {:comment-open-tag ";" :comment-close-tag "\n" :quote-open-tag "\""})
  ; =>
  ; {:commented [[10 24]] :quoted [[25 37]]}
  ;
  ; @example
  ; (grey-zones "My string ; My comment 1\n ; My comment 2\n" {:comment-open-tag ";" :comment-close-tag "\n" :quote-open-tag "\""})
  ; =>
  ; {:commented [[10 26] [27 43]]
  ;
  ; @return (map)
  ; {:commented (integer pairs in vectors in vector)
  ;  :quoted (integer pairs in vectors in vector)}
  [n {:keys [comment-close-tag quote-close-tag] :as tags} options]
  (letfn [; - Returns a vector with the updated 'grey-zones' map and 'tag-positions' map.
          ; - Adds the first commented / quoted zone's boundaries (as an integer pair in a vector) as a new zone to the ':commented' / ':quoted' vector
          ;   in the 'grey-zones' map and updates the 'tag-positions' map by removing that boundaries from it.
          ; - It calculates the zone's end boundary by adding the closing tag's length to the closing tag's position.
          ; - If there is no ':comment-close-tag' / ':quote-close-tag' passed to the main function it uses the ':comment-open-tag' / ':quote-open-tag'
          ;   as a closing tag and it works like that is every second item in the ':comment-open-tag' / ':quote-open-tag' positions vector is a closing position.
          ; - The passed 'keys' vector determines whether this function works on a commented zone or a quoted zone.
          ; - The 'f1' function only applies this ('f0') function if there is at least one item in the ':comment-open-tag' / ':quote-open-tag' vector
          ;   therefore in this function there is no need to check whether the first item is presented in that vectors.
          ;
          ; @param (integer pairs in vectors in vector) grey-zones
          ; @param (map) tag-positions
          ; @param (keywords in vector) keys
          ;
          ; @example
          ; (f0 {:commented []} {:comment-open-tag [12 16 42 46 56 64]} [:comment-open-tag :comment-close-tag :comments])
          ; =>
          ; [{:commented [12 17]} {:comment-open-tag [42 46 56 64]}]
          ;
          ; @example
          ; (f0 {:commented []} {:comment-open-tag [12 42 56] :comment-close-tag [16 46 64]} [:comment-open-tag :comment-close-tag :comments])
          ; =>
          ; [{:commented [12 17]} {:comment-open-tag [42 56] :comment-close-tag [46 64]}]
          (f0 [grey-zones tag-positions [open-tag-key close-tag-key zone-name-key]]
              (cond ; If the ':comment-close-tag' / ':quote-close-tag' is passed ...
                    (some? comment-close-tag)
                    (let [zone-open  (-> tag-positions open-tag-key  first)
                          zone-close (-> tag-positions close-tag-key first)]
                         (if zone-close [(-> grey-zones    (update zone-name-key vector/conj-item [zone-open (+ zone-close (count comment-close-tag))]))
                                         (-> tag-positions (update open-tag-key  subvec 1)
                                                           (update close-tag-key subvec 1))]
                                        [(-> grey-zones)
                                         (-> tag-positions (assoc open-tag-key  [])
                                                           (assoc close-tag-key []))]))
                    ; If the ':comment-close-tag' / ':quote-close-tag' is NOT passed ...
                    :using-the-opening-tag-as-a-closing-tag
                    (let [zone-open  (-> tag-positions open-tag-key first)
                          zone-close (-> tag-positions open-tag-key second)]
                         (if zone-close [(-> grey-zones    (update zone-name-key vector/conj-item [zone-open (+ zone-close (count comment-open-tag))]))
                                         (-> tag-positions (update open-tag-key subvec 2))]
                                        [(-> grey-zones)
                                         (-> tag-positions (assoc open-tag-key []))]))))

          ; - ...
          ; - It only applies the 'f0' function if there is at least one item in the ':comment-open-tag' / ':quote-open-tag' vector.
          (f1 [grey-zones tag-positions]
              (let [first-comment-open (-> tag-positions :comment-open-tag first)
                    first-quote-open   (-> tag-positions :quote-open-tag   first)]
                   (cond ; If there is no more comment / quote opening position in the 'tag-positions' map.
                         (and (nil? first-comment-open)
                              (nil? first-quote-open))
                         (-> grey-zones)
                         ; If there is only a comment opening position in the 'tag-positions' map.
                         (nil? first-quote-open)
                         (let [[grey-zones tag-positions] (f0 grey-zones tag-positions [:comment-open-tag :comment-close-tag :comments])]
                              (f1 grey-zones tag-positions))
                         ; If there is only a quote opening position in the 'tag-positions' map.
                         (nil? first-comment-open)
                         (let [[grey-zones tag-positions] (f0 grey-zones tag-positions [:quote-open-tag :quote-close-tag :quotes])]
                              (f1 grey-zones tag-positions))
                         ; If the first commented zone starts earlier than the first quoted zone in the 'tag-positions' map.
                         (< first-comment-open first-quote-open)
                         (let [[grey-zones tag-positions] (f0 grey-zones tag-positions [:comment-open-tag :comment-close-tag :comments])]
                              (f1 grey-zones tag-positions))
                         ; If the first quoted zone starts earlier than the first commented zone in the 'tag-positions' map.
                         (> first-comment-open first-quote-open)
                         (let [[grey-zones tag-positions] (f0 grey-zones tag-positions [:quote-open-tag :quote-close-tag :quotes])]
                              (f1 grey-zones tag-positions))
                         ; There is no possible outcome where two positions could be the same!
                         (= first-comment-open first-quote-open)
                         :the-tag-positions-function-returns-a-positions-map-with-no-duplicated-positions)))]

         ; ...
         (f1 [] (tag-positions n tags options))))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn interpreter
  ; @param (string) n
  ; @param (map) options
  ; {:ignore-comments? (boolean)(opt)
  ;   To use this option, provide the ':comment' value in the ':tags' property!
  ;   Default: false
  ;  :ignore-quotes? (boolean)(opt)
  ;   To use this option, provide the ':quote' value in the ':tags' property!
  ;   Default: false
  ;  :ignore-escaped? (boolean)(opt)
  ;   Default: true
  ;  :tags (map)
  ;   {:my-tag-name (strings in vector)
  ;     [(string) open-tag
  ;      (string) close-tag]}}
  ;   Default: {:open-bracket  "["
  ;             :close-bracket "]"
  ;             :open-paren    "("
  ;             :close-paren   ")"
  ;             :comment       ";"
  ;             :newline       "\n"
  ;             :quote         "\""}}
  ;
  ; @usage
  ; "abc"
  [n {:keys [tags] :or {tags {:open-bracket "[" :close-bracket "]"
                              :open-paren   "(" :close-paren   ")"
                              :comment ";"
                              :newline "\n"
                              :quote   "\""}}}]
  (tag-positions n tags {}))
