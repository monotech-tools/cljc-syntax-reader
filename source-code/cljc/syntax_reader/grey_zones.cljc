
(ns syntax-reader.grey-zones
    (:require [string.api                :as string]
              [syntax-reader.interpreter :as interpreter]
              [vector.api                :as vector]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn grey-zones
  ; @description
  ; Returns the ranges (zone boundaries) in the given 'n' string that are commented / quoted and the cursor positions that are escaped.
  ;
  ; @param (string) n
  ; @param (vectors in map)(opt) tags
  ; {:comment (vector)
  ;   [(string) opening-tag
  ;    (string) closing-tag
  ;    (map)(opt) tag-options
  ;     {:disable-interpreter? (boolean)(opt)}
  ;       Default: false}]
  ;  :quote (vector)
  ;   [(string) opening-tag
  ;    (string) closing-tag
  ;    (map)(opt) tag-options
  ;     {:disable-interpreter? (boolean)(opt)}
  ;       Default: false}]}
  ; Default: {:comment [";" "\n"  {:disable-interpreter? true}]
  ;           :quote   ["\"" "\"" {:disable-interpreter? true}]}
  ; @param (map)(opt) options
  ; {:endpoint (integer)(opt)
  ;   Stops the searching at the endpoint position in the given 'n' string.
  ;  :offset (integer)(not available)
  ;   It would be great to use an offset value where the search could start in the given 'n' string,
  ;   but in order to make accurate grey-zone map (commented / quoted parts) the search must start at the beginning of the string.}
  ;
  ; @usage
  ; (grey-zones "My string" {...})
  ;
  ; @usage
  ; (grey-zones "My string ; My comment\n \"My quote\"")
  ;
  ; @example
  ; (grey-zones "My string ; My comment\n \"My quote\"")
  ; =>
  ; {:commented [[11 22]] :quoted [[27 35]]}
  ;
  ; @example
  ; (grey-zones "My string ; My comment 1\n ; My comment 2\n")
  ; =>
  ; {:commented [[11 24] [28 41]]
  ;
  ; @return (map)
  ; {:commented (integer pairs in vectors in vector)
  ;   [[(integer) zone-start
  ;     (integer) zone-end]]
  ;  :escaped (integers in vector)
  ;   [(integer) escaped-position]
  ;  :quoted (integer pairs in vectors in vector)
  ;   [[(integer) zone-start
  ;     (integer) zone-end]]}
  ([n]
   (grey-zones n (select-keys config/DEFAULT-TAGS [:comment :quote]) {}))

  ([n tags]
   (grey-zones n tags {}))

  ([n tags options]
   (letfn [; @description
           ; Returns whether the actual cursor position is commented / quoted.
           ;
           ; @example
           ; (grey-zones "My string ; My comment\n \"My quote\"")
           ;
           ; (f0 [[:comment 1 11]]) ; <- A ':comment' tag opened at the 11th cursor position.
           ; =>
           ; :commented
           (f0 [tag-map]
               (some (fn [[tag-name _ _]] (case tag-name :comment :commented :quote :quoted nil))
                     (-> tag-map)))

           ;...
           (f1 [{:keys [commented quoted] :as result} cursor tag-map]
             (let [zone-name (f0 tag-map)]
                  (cond ; If there is at least one vector in the ':commented' vector in the output map,
                        ; and it doesn't contain its second value (missing end boundary = still opened comment) ...
                        (and (-> result :commented last vector?)
                             (-> result :commented last second nil?))
                        (if (not= :commented zone-name)
                            ; If the actual cursor position is not commented, that means the commented zone
                            ; ended at the previous cursor position, so its time to store the end boundary.
                            (update-in result [:commented (seqable/last-dex commented)] vector/conj-item (dec cursor))
                            ; If the actual cursor position is still commented, it returns the unchanged output map.
                            (-> result))

                        ; If there is at least one vector in the ':quoted' vector in the output map,
                        ; and it doesn't contain its second value (missing end boundary = still opened quote) ...
                        (and (-> result :quoted last vector?)
                             (-> result :quoted last second nil?))
                        (if (not= :quoted zone-name)
                            ; If the actual cursor position is not quoted, that means the quoted zone
                            ; ended at the previous cursor position, so its time to store the end boundary.
                            (update-in result [:quoted (seqable/last-dex quoted)] vector/conj-item (dec cursor))
                            ; If the actual cursor position is still commented, it returns the unchanged output map.
                            (-> result))

                        ; If the previous conditions aren't match, that means no commented / quoted zone is opened
                        ; at the actual cursor position, so its time check whether something opens at the actual cursor position ...
                        (-> zone-name)
                        ; If the 'f0' function found something, it updates the output map by adding a vector with (only) the start boundary
                        ; of the found commented / quoted zone.
                        (update result zone-name vector/conj-item [cursor])

                        ; ...
                        :return result)))]

          ; ...
          (let [initial {:commented [] :escaped? [] :quoted []}]
               (interpreter n f1 initial tags options)))))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn remove-commented-parts
  ; @description
  ; Removes the commented parts from the given 'n' string.
  ;
  ; @param (string) n
  ; @param (map)(opt) tags
  ; {:comment-close-tag (string)(opt)
  ;   Default: "\n"
  ;  :comment-open-tag (string)(opt)
  ;   Default: ";"
  ;  :quote-close-tag (string)(opt)
  ;  :quote-open-tag (string)(opt)
  ;   Default: "\""}
  ; @param (map)(opt) options
  ; {:ignore-quoted? (boolean)(opt)
  ;   TODO
  ;   Default: true
  ;  :ignore-escaped? (boolean)(opt)
  ;   Default: true}
  ;
  ; @usage
  ; (remove-commented-parts "(defn my-function [])\n ; My comment\n")
  ;
  ; @example
  ; (remove-commented-parts "(defn my-function [])\n ; My comment\n")
  ; =>
  ; "(defn my-function [])\n "
  ;
  ; @example
  ; (remove-commented-parts "body { /* My comment */ color: blue; }"
  ;                         {:comment-open-tag "/*"
  ;                          :comment-close-tag "*/"})
  ; =>
  ; "body {  color: blue; }"
  ;
  ; @return (string)
  ([n]
   (remove-commented-parts n {} {}))

  ([n tags]
   (remove-commented-parts n tags {}))

  ([n tags options]
   (let [grey-zones (interpreter/grey-zones n tags options)]
        (letfn [; - If there is at least one commented zone in the 'commented-zones' vector
                ;   it uses the first commented zone's boundaries to cut out that zone from the 'result' string
                ;   and calls itself recursivelly (while dropping the first zone from the 'comment-zones' vector).
                ; - If there is no more commented zone left in the 'commented-zones' vector it returns the result.
                ; - It uses the offset to determine how many characters are already cut out from the result to
                ;   adjust to cutting boundaries in every iteration.
                (f [result offset [[zone-start zone-end :as commented-zone] _ :as commented-zones]]
                   (if commented-zone (f (string/cut-range result (- zone-start offset) (- zone-end offset))
                                         (+ offset (- zone-end zone-start))
                                         (vector/remove-first-item commented-zones))
                                      (-> result)))]
               ; ...
               (f n 0 (:commented grey-zones))))))
