
(ns syntax-reader.interpreter
    (:require [string.api           :as string]
              [syntax-reader.check  :as check]
              [syntax-reader.config :as config]
              [syntax-reader.utils  :as utils]
              [vector.api           :as vector]
              [seqable.api :as seqable]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn interpreter
  ; @description
  ; Applies the given 'f' function at each cursor position of the given 'n' string
  ; providing it a tag map that describes the actual opened tags and their depths at the cursor position.
  ;
  ; @param (string) n
  ; @param (function) f
  ; Applied at each cursor position.
  ; Takes the output of the previous iteration (or the given 'initial' value) as first parameter.
  ; Takes the actual 'cursor' value as second parameter.
  ; Takes the tag map for the actual cursor position as third parameter.
  ; @param (*) initial
  ; @param (vectors in map)(opt) tags
  ; {:my-tag (vector)
  ;   [(string) opening-tag
  ;    (string) closing-tag
  ;    (map)(opt) tag-options
  ;     {:disable-interpreter? (boolean)(opt)}
  ;       Default: false}]}
  ; Default: {:brace   ["{" "}"]
  ;           :bracket ["[" "]"]
  ;           :comment [";" "\n" {:disable-interpreter? true}]
  ;           :paren   ["(" ")"]
  ;           :quote   ["\"" "\"" {:disable-interpreter? true}]}
  ; @param (map)(opt) options
  ; {:endpoint (integer)(opt)
  ;   Stops the iteration at the endpoint position in the given 'n' string.
  ;  :ignore-escaped? (boolean)(opt)
  ;   Default: true
  ;  :offset (integer)(opt)
  ;   Starts the iteration at the offset position in the given 'n' string.
  ;   Using an 'offset' value might cause inaccurate position map!}
  ;
  ; @usage
  ; (defn my-function [result cursor tag-map] ...)
  ; (interpreter "My string" my-function {})
  ;
  ; @example
  ; (defn my-function [result cursor tag-map] (if (= cursor 16) tag-map result))
  ; (interpreter "(-> my-value (= 420))" my-function nil)
  ; =>
  ; [[:paren 1  1]
  ;  [:paren 2 14]]
  ;
  ; @return (*)
  ([n f initial]
   (interpreter n f initial config/DEFAULT-TAGS {}))

  ([n f initial tags]
   (interpreter n f initial tags {}))

  ([n f initial tags {:keys [endpoint ignore-escaped? offset] :or {endpoint (count n) offset 0}}]
   (letfn [; @description
           ; If the interpreter is disabled by an opened tag, it returns the disabling 'tag-name'.
           ;
           ; @param (map) state
           ; {:tag-map (vectors in vector)}
           ;
           ; @example
           ; (let [n "(abc(def ;ghi\n))"
           ;       f (fn [_ _ _])]
           ;      (interpreter n f nil))
           ;
           ; (f0 {:tag-map [[:paren   1  1]
           ;                [:paren   2  5]
           ;                [:comment 1 10]]}) ; <- A ':comment' tag opened at the 10th cursor position and it is currently disabling the interpreter.
           ; =>
           ; :comment
           ;
           ; @return (keyword)
           (f0 [{:keys [tag-map]}]
               (if-let [tag-name (-> tag-map last first)]
                       (if (get-in tags [tag-name 2 :disable-interpreter?])
                           (-> tag-name))))

           ; @description
           ; Returns the actual depth of the given 'tag-name'.
           ;
           ; @param (map) state
           ; {:tag-map (vectors in vector)}
           ; @param (keyword) tag-name
           ;
           ; @example
           ; (let [n "(abc(def ;ghi\n))"
           ;       f (fn [_ _ _])]
           ;      (interpreter n f nil))
           ;
           ; (f1 {:tag-map [[:paren   1  1]
           ;                [:paren   2  5]
           ;                [:comment 1 10]]}
           ;     :paren)
           ; =>
           ; 2
           ;
           ; @return (integer)
           (f1 [{:keys [tag-map]} tag-name]
               (vector/match-count tag-map #(-> % first (= tag-name))))

           ; @description
           ; Returns the 'tag-name' if any opening tag ends at the actual cursor position.
           ;
           ; @param (map) state
           ; {:tag-map (vectors in vector)}
           ; @param (integer) cursor
           ;
           ; @example
           ; (let [n "(abc(def ;ghi\n))"
           ;       f (fn [_ _ _])]
           ;      (interpreter n f nil))
           ;
           ; (f2 {...} 1) ; <- Applying the 'f2' function at the 1st cursor position of the given "(abc(def ;ghi\n))" string...
           ; =>
           ; :paren       ; <- 1st cursor position: a ':paren' opening tag ends.
           ;
           ; @return (keyword)
           (f2 [_ cursor]
               (some (fn [[tag-name [opening-tag _]]] (if (string/ends-at? n opening-tag cursor) tag-name))
                     (-> tags)))

           ; @description
           ; Returns the 'tag-name' if any closing tag starts at the previous cursor position.
           ;
           ; @param (map) state
           ; {:tag-map (vectors in vector)}
           ; @param (integer) cursor
           ;
           ; @example
           ; (let [n "(abc(def ;ghi\n))"
           ;       f (fn [_ _ _])]
           ;      (interpreter n f nil))
           ;
           ; (f3 {...} 16) ; <- Applying the 'f3' function at the 16th cursor position of the given "(abc(def ;ghi\n))" string ...
           ; =>
           ; :paren        ; <- 16th cursor position: a ':paren' closing tag started at the previous (4th) cursor position.
           ;
           ; @return (keyword)
           (f3 [_ cursor]
               ; The cursor position must be a positive integer after decreasing it by one, otherwise the 'string/starts-at?'
               ; function would search at the other end of the provided string (because of the negative cursor position).
               (if (< 0 cursor)
                   (some (fn [[tag-name [_ closing-tag]]] (if (string/starts-at? n closing-tag (dec cursor)) tag-name))
                         (-> tags))))

           ; @description
           ; Updates the given 'state' if any ...
           ; ... opening tag ends at the actual cursor position.
           ; ... closing tag starts at the previous cursor position.
           ;
           ; @param (map) state
           ; {:tag-map (vectors in vector)}
           ; @param (integer) cursor
           ;
           ; @usage
           ; (let [n "(abc(def ;ghi\n))"
           ;       f (fn [_ _ _])]
           ;      (interpreter n f nil))
           ;
           ; (f4 {:tag-map [[:paren   1  1]
           ;                [:paren   2  5]
           ;                [:comment 1 10]]}
           ;     12) ; <- 12th cursor position: the interpreter is disabled since the 10th cursor position by the ':comment' tag.
           ;
           ; @return (map)
           (f4 [{:keys [tag-map] :as state} cursor]
               (or ; The very first condition must be whether the interpreter is currently disabled by any opened tag.
                   (if-let [disabling-tag-name (f0 state)]
                           ; If the interpreter is disabled by the last processed (not the last found) tag ...
                           (if-let [found-closing-tag (f3 state cursor)]
                                   ; If a closing tag starts at the previous cursor position ...
                                   (if (= disabling-tag-name found-closing-tag)
                                       ; If the found closing tag corresponds to the opening tag that disabled the interpreter before ...
                                       ; ... it removes the last opened tag (the one that disabled the interpreter) from the tag map.
                                       (update state :tag-map vector/before-last-match #(-> % first (= found-closing-tag)) {:return? true})))
                           ; If the interpreter is not disabled ...
                           (if-let [found-opening-tag (f2 state cursor)]
                                   ; If an opening tag ends at the actual cursor position ...
                                   ; ... it adds the new tag to the tag map.
                                   (let [depth (f1 state found-opening-tag)]
                                        (update state :tag-map vector/conj-item [found-opening-tag (inc depth) cursor]))
                                   ; If NO opening tag ends at the actual cursor position ...
                                   ; ... it searches for closing tags.
                                   (if-let [found-closing-tag (f3 state cursor)]
                                           ; - If a closing tag starts at the previous cursor position ...
                                           ;   ... it removes the just closed tag from the tag map.
                                           ; - Removes the following (the rest) tags also (in case of somehow they aren't closed yet).
                                           ; - Uses the '{:return? true}' setting for the 'vector/before-last-match' function to make sure
                                           ;   it doesn't empty the whole tag map if it doesn't contain any opened tag with the given tag name.
                                           ;   E.g., In Clojure comments end with a newline character but not every newline character means that
                                           ;         there is any opened comment tag to close.
                                           (update state :tag-map vector/before-last-match #(-> % first (= found-closing-tag)) {:return? true}))))
                   ; If nothing happened at the actual cursor position, it returns the unchanged state ...
                   (-> state)))

           ; ...
           (f5 [{:keys [result tag-map] :as state} cursor]
               (cond ; Nomen est omen. If no more cursor position to examine in the given 'n' string, it returns the last output of the applied given 'f' function ...
                     (seqable/cursor-out-of-bounds? n cursor)
                     (-> result)
                     ; If the 'stop' function stopped the iteration by wrapping the 'result' value in a vector,
                     ; and putting the '::$stop' marker into it as its first item ...
                     (-> result vector? (and (-> result first (= ::$stop))))
                     (-> result second)
                     ; If the cursor reached the given 'endpoint' in the given 'n' string ...
                     (= cursor endpoint)
                     (let [state  (f4 state cursor)
                           result (f result cursor (:tag-map state))]
                          (-> result))
                     ; If everything is normal, it updates the state, then applies the given 'f' function and calls itself recursivelly ...
                     :else
                     (let [state  (f4 state cursor)
                           result (f result cursor (:tag-map state))]
                          (f5 (assoc state :result result)
                              (inc cursor)))))]

          ; ...
          (f5 {:result nil :tag-map nil} offset))))

(defn stop
  ; @description
  ; Use this 'stop' metafunction to wrap your return value with it in your function
  ; and the interpreter will stop immediatelly and return your return value.
  ;
  ; @param (*) result
  ;
  ; @usage
  ; (defn my-function
  ;   [result cursor tag-map]
  ;   (let [result (assoc result :my-value "My value")]
  ;        (if (not= cursor 4)
  ;            result           ; <- Lets the interpreter run the next iteration and returns the result.
  ;            (stop result)))) ; <- Stops the interpreter at the actual cursor position and returns the result.
  ; (interpreter "My string" my-function nil)
  ;
  ; @return (vector)
  [result]
  [::$stop result])

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn test
  []
  ;(println "r:" (tag-first-position "abc()")))
  ;(println "r:" (grey-zones)))
  (println
   (grey-zones
   ;(string/keep-range)
    (io.api/read-file "dependencies/ajax-api/source-code/cljs/ajax/api.cljs"))))
     ;0 150)
   ;println nil))

(defn tag-first-position
  ; @description
  ; Returns the position of the first occurence of the 'tag' string in the 'n' string.
  ;
  ; @param (string) n
  ; @param (string) tag
  ; @param (map)(opt) tags
  ; {:comment-closing-tag (string)(opt)
  ;   Default: "\n"
  ;  :comment-opening-tag (string)(opt)
  ;   Default ";"
  ;  :quote-closing-tag (string)(opt)
  ;   Default: "\""
  ;  :quote-opening-tag (string)(opt)
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

  ([n tag tags]
   (tag-first-position n tag tags {}))

  ([n tag tags options]
   (letfn [(f [_ cursor {:keys [commented? escaped? quoted?]}]
              ; The 'commented?' / 'escaped?' / 'quoted?' value remains NIL in case of the given
              ; ':ignore-commented?' / ':ignore-escaped?' / ':ignore-quoted?' property is FALSE.
              (if-not (or commented? escaped? quoted?)
                      (if (string/starts-at? n "(" cursor)
                          (stop cursor))))]
          ; ...
          (let [initial nil]
               (interpreter n initial tags options)))))

(defn closing-tag-position
  ; @description
  ; Returns the position of the corresponding closing tag of the first occurence of the 'opening-tag' string in the 'n' string.
  ;
  ; @param (string) n
  ; @param (string) opening-tag
  ; @param (string) closing-tag
  ; {:comment-closing-tag (string)(opt)
  ;   Default: "\n"
  ;  :comment-opening-tag (string)(opt)
  ;   Default ";"
  ;  :quote-closing-tag (string)(opt)
  ;   Default: "\""
  ;  :quote-opening-tag (string)(opt)
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
  ; (closing-tag-position "<div>My content</div>" "<div>" "</div>")
  ; =>
  ; 15
  ;
  ; @example
  ; (closing-tag-position "<div><div></div></div>" "<div>" "</div>")
  ; =>
  ; 16
  ;
  ; @example
  ; (closing-tag-position "</div> <div></div>" "<div>" "</div>")
  ; =>
  ; 12
  ;
  ; @return (integer)
  ([n opening-tag closing-tag]
   (closing-tag-position n opening-tag closing-tag {} {}))

  ([n opening-tag closing-tag tags]
   (closing-tag-position n opening-tag closing-tag tags {}))

  ([n opening-tag closing-tag tags options]
   (letfn [(f [{:keys [depth] :as xx} cursor {:keys [commented? escaped? quoted?]}])]
              ; The 'commented?' / 'escaped?' / 'quoted?' value remains NIL in case of the given
              ; ':ignore-commented?' / ':ignore-escaped?' / ':ignore-quoted?' property is FALSE.
              ;(cond (or commented? escaped? quoted?)
              ;      (->)]
              ;(if-not (or commented? escaped? quoted?)
              ;        (if (= depth 1))])]
          ; ...
          (let [initial {:depth 0}]
               (interpreter n f initial tags options)))))


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
  ; {:endpoint (integer)(opt)
  ;   Stops the searching at the endpoint position in the given 'n' string.
  ;  :ignore-escaped? (boolean)(opt)
  ;   Default: true
  ;  :offset (integer)(opt)
  ;   Starts the searching at the offset position in the given 'n' string
  ;   (the returned positions remain absolute positions from the beginning of the string).
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

  ([n tags {:keys [endpoint ignore-escaped? limit offset] :or {endpoint (count n) ignore-escaped? true offset 0}}]
   (letfn [; Returns TRUE if all the searched tags' found occurence count exceeded the limit value.
           (f0 [state] (every? (fn [[tag-name _]] (-> state tag-name (vector/count? limit))) tags))

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
                     (update state tag-name vector/conj-item (+ cursor offset))))

           ; If the 'f1' function found something and returned an updated state it returns the updated state,
           ; otherwise iz returns the original / given state.
           (f2 [state cursor] (or (some #(f1 state cursor %) tags) state))]

          ; Iterates over the given 'n' string and applies the 'f1' function at every cursor position.
          (-> n (string/keep-range offset endpoint)
                (string/walk {} f2)))))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn grey-zones_
  ; @description
  ; - Returns the ranges (zone boundaries) in the given 'n' string that are commented / quoted.
  ; - If the ':comment-close-tag' / ':quote-close-tag' is not passed it uses the comment / quote opening tags as closing tags.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; {:comment-close-tag (string)(opt)
  ;  :comment-open-tag (string)
  ;  :quote-close-tag (string)(opt)
  ;  :quote-open-tag (string)}
  ; @param (map)(opt) options
  ; {:endpoint (integer)(opt)
  ;   Stops the searching at the endpoint position in the given 'n' string.
  ;  :ignore-escaped? (boolean)(opt)
  ;   Default: true
  ;  :offset (integer)(not available)
  ;   It would be great to use an offset value where the search could start in the given 'n' string,
  ;   but in order to make accurate grey-zone map (commented / quoted parts) the search must start at the beginning of the string.}
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
                ;   therefore in this function there is no need to check whether the first item is presented in that vector.
                ;
                ; @param (integer pairs in vectors in vector) grey-zones
                ; @param (map) tag-positions
                ; @param (keywords in vector) keys
                ; [:comment-open-tag :comment-close-tag :commented], [:quote-open-tag :quote-close-tag :quoted]
                ;
                ; @example
                ; (f0 {:commented []} {:comment-open-tag [12 16 42 46 56 64]} [:comment-open-tag :comment-close-tag :commented])
                ; =>
                ; [{:commented [12 17]} {:comment-open-tag [42 46 56 64]}]
                ;
                ; @example
                ; (f0 {:commented []} {:comment-open-tag [12 42 56] :comment-close-tag [16 46 64]} [:comment-open-tag :comment-close-tag :commented])
                ; =>
                ; [{:commented [12 17]} {:comment-open-tag [42 56] :comment-close-tag [46 64]}]
                (f0 [grey-zones tag-positions [open-tag-key close-tag-key zone-name-key]]
                    (let [; It uses the 'open-tag-key' as 'close-tag-key' also, if the given 'tags' map doesn't contain a tag under tag 'close-tag-key' key.
                          ; E.g., tags: {:quote-open-tag "\"" :quote-close-tag nil}
                          close-tag-key (if (-> tags close-tag-key) close-tag-key open-tag-key)
                          ; ...
                          zone-start (-> tag-positions open-tag-key  (vector/first-item))
                          zone-end   (-> tag-positions close-tag-key (vector/first-filtered #(> % zone-start)))]
                         (if zone-end [; Adding the first zone boundaries to the 'grey-zones' vector and removing the positions that are smaller than the zone end from the 'tag-positions' vector.
                                       (-> grey-zones    (update zone-name-key vector/conj-item [zone-start (+ zone-end (-> tags close-tag-key count))]))
                                       (-> tag-positions (update open-tag-key  vector/remove-items-by #(<= % zone-end))
                                                         (update close-tag-key vector/remove-items-by #(<= % zone-end)))]
                                      [; If no zone end is found ...
                                       (-> grey-zones)
                                       (-> tag-positions (assoc open-tag-key  [])
                                                         (assoc close-tag-key []))])))

                ; - ...
                ; - It only applies the 'f0' function if there is at least one item in the ':comment-open-tag' / ':quote-open-tag' vector.
                (f1 [grey-zones tag-positions]
                    (let [first-comment-open (-> tag-positions :comment-open-tag first)
                          first-quote-open   (-> tag-positions :quote-open-tag   first)]
                         (cond ; If there is no more comment / quote opening position in the 'tag-positions' map ...
                               ; ... it returns the 'grey-zones' map as result.
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
                               :the-tag-positions-function-must-return-a-positions-map-with-no-duplicated-positions)))]

               ; ...
               (f1 {} (tag-positions n tags options))))))
