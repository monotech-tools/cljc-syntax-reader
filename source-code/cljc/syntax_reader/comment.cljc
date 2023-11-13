
(ns syntax-reader.comment
    (:require [string.api                :as string]
              [syntax-reader.interpreter :as interpreter]
              [vector.api                :as vector]))

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
