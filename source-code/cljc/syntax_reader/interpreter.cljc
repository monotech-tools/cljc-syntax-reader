
(ns syntax-reader.interpreter
    (:require [string.api          :as string]
              [syntax-reader.check :as check]
              [syntax-reader.utils :as utils]
              [vector.api          :as vector]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn tag-positions
  ; @warning
  ; This function doesn't collect duplicated tag occurences in case of more than one tag
  ; has the same value for searching!
  ;
  ; @description
  ; Returns a map that contains all the positions of the given tags found in the given 'n' string.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; {:my-tag-name (string)}
  ; @param (map)(opt) options
  ; {:ignore-escaped? (boolean)(opt)
  ;   Default: true
  ;  :limit (integer)(opt)
  ;   Stops searching for further occurences in case of the already found occurence count exceeds the 'limit' value.}
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

  ([n tags {:keys [ignore-escaped? limit] :or {ignore-escaped? true}}]
   (letfn [; Returns TRUE if all the searched tags' found occurence count exceeded the limit value.
           (f0 [state] (every? #(vector/count? (get state (first %)) limit) tags))

           ; If the given tag starts at the actual cursor it updates the state and returns it, otherwise it returns NIL.
           (f1 [state cursor [tag-name tag]]
               (cond ; Returns NIL if the 'limit' option is provided and ...
                     (and limit (f0 state))
                     (-> nil)
                     ; Returns NIL if the 'ignore-escaped?' option is TRUE and an escape character preceeds the cursor.
                     (and ignore-escaped? (check/position-escaped? n cursor))
                     (-> nil)
                     ; Returns the updated state if the given 'tag' string starts at the given 'cursor' value in the 'n' string.
                     (string/starts-at? n tag cursor)
                     (update state tag-name vector/conj-item cursor)))

           ; If the 'f1' function found something and returned an updated state it returns the updated state,
           ; otherwise iz returns the original / given state.
           (f1 [state cursor] (or (some #(f1 state cursor %) tags) state))]

          ; Iterates over the given 'n' string and applies the 'f1' function at every cursor position.
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
  ; @param (map)(opt) options
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
  ;   [[(integer) zone-start
  ;     (integer) zone-end]]
  ;  :quoted (integer pairs in vectors in vector)
  ;   [[(integer) zone-start
  ;     (integer) zone-end]]}
  ([n]
   (grey-zones n {} {}))

  ([n tags]
   (grey-zones n tags {}))

  ([n tags options]
   (let [tags (-> tags utils/default-comment-tags utils/default-quote-tags)]
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
                          (-> tags close-tag-key some?)
                          (let [zone-start (-> tag-positions open-tag-key first)
                                zone-end   (-> tag-positions close-tag-key (vector/first-filtered #(> % zone-start)))]
                               (if zone-end [(-> grey-zones    (update zone-name-key vector/conj-item [zone-start (+ zone-end (-> tags close-tag-key count))]))
                                             (-> tag-positions (update open-tag-key  vector/remove-items-by #(<= % zone-end))
                                                               (update close-tag-key vector/remove-items-by #(<= % zone-end)))]
                                            [(-> grey-zones)
                                             (-> tag-positions (assoc open-tag-key  [])
                                                               (assoc close-tag-key []))]))
                          ; If the ':comment-close-tag' / ':quote-close-tag' is NOT passed ...
                          :using-the-opening-tag-as-a-closing-tag
                          (let [zone-start (-> tag-positions open-tag-key first)
                                zone-end   (-> tag-positions open-tag-key (vector/first-filtered #(> % zone-start)))]
                               (if zone-end [(-> grey-zones    (update zone-name-key vector/conj-item [zone-start (+ zone-end (-> tags open-tag-key count))]))
                                             (-> tag-positions (update open-tag-key  vector/remove-items-by #(<= % zone-end)))]
                                            [(-> grey-zones)
                                             (-> tag-positions (assoc open-tag-key []))]))))

                ; - ...
                ; - It only applies the 'f0' function if there is at least one item in the ':comment-open-tag' / ':quote-open-tag' vector.
                (f1 [grey-zones tag-positions]
                    (let [first-comment-open (-> tag-positions :comment-open-tag first)
                          first-quote-open   (-> tag-positions :quote-open-tag   first)]
                         (cond ; If there is no more comment / quote opening position in the 'tag-positions' map ...
                               ; ... it returns the 'grey-zones' map.
                               (and (nil? first-comment-open)
                                    (nil? first-quote-open))
                               (-> grey-zones)
                               ; If there is only a COMMENT opening position in the 'tag-positions' map ...
                               ; ... uses the 'f0' function to derive the first COMMENTED zone's boundaries from the 'tag-positions' map.
                               ; ... calls itself recursivelly.
                               (nil? first-quote-open)
                               (let [[grey-zones tag-positions] (f0 grey-zones tag-positions [:comment-open-tag :comment-close-tag :commented])]
                                    (f1 grey-zones tag-positions))
                               ; If there is only a QUOTE opening position in the 'tag-positions' map ...
                               ; ... uses the 'f0' function to derive the first QUOTED zone's boundaries from the 'tag-positions' map.
                               ; ... calls itself recursivelly.
                               (nil? first-comment-open)
                               (let [[grey-zones tag-positions] (f0 grey-zones tag-positions [:quote-open-tag :quote-close-tag :quoted])]
                                    (f1 grey-zones tag-positions))
                               ; If the first COMMENTED zone starts earlier than the first QUOTED zone in the 'tag-positions' map ...
                               ; ... uses the 'f0' function to derive the first COMMENTED zone's boundaries from the 'tag-positions' map.
                               ; ... calls itself recursivelly.
                               (< first-comment-open first-quote-open)
                               (let [[grey-zones tag-positions] (f0 grey-zones tag-positions [:comment-open-tag :comment-close-tag :commented])]
                                    (f1 grey-zones tag-positions))
                               ; If the first QUOTED zone starts earlier than the first COMMENTED zone in the 'tag-positions' map ...
                               ; ... uses the 'f0' function to derive the first QUOTED zone's boundaries from the 'tag-positions' map.
                               ; ... calls itself recursivelly.
                               (> first-comment-open first-quote-open)
                               (let [[grey-zones tag-positions] (f0 grey-zones tag-positions [:quote-open-tag :quote-close-tag :quoted])]
                                    (f1 grey-zones tag-positions))
                               ; There is no possible outcome where two positions are the same in the output of the 'tag-positions' function!
                               (= first-comment-open first-quote-open)
                               :the-tag-positions-function-returns-a-positions-map-with-no-duplicated-positions)))]

               ; ...
               (f1 {} (tag-positions n tags options))))))
