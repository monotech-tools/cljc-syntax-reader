
(ns syntax-reader.comments.engine
    (:require [fruits.seqable.api              :as seqable]
              [fruits.string.api               :as string]
              [fruits.vector.api               :as vector]
              [syntax-reader.comments.utils    :as comments.utils]
              [syntax-reader.grey-zones.engine :as grey-zones.engine]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn remove-commented-parts
  ; @description
  ; Removes the commented parts from the given 'n' string.
  ;
  ; @param (string) n
  ; @param (vectors in vector)(opt) tags
  ; Default:
  ; [[:comment #"\;" #"\n" {:disable-interpreter? true}]
  ;  [:quote   #"\"" #"\"" {:disable-interpreter? true}]]
  ; @param (map)(opt) options
  ; {:endpoint (integer)(opt)
  ;   Quits removing commented parts at the given 'endpoint' position in the given 'n' string.
  ;  :keep-indents? (boolean)(opt)
  ;   Default: false
  ;  :offset (integer)(opt)
  ;   Starts removing commented parts from the given 'offset' position in the given 'n' string.
  ;  :remove-leftover-blank-lines? (boolean)(opt)
  ;   Default: false
  ;  :trim-line-ends? (boolean)(opt)
  ;   TODO
  ;   Default: false}
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
  ;                         {:comment [#"/\*.*\*"})
  ; =>
  ; "body {  color: blue; }"
  ;
  ; @return (string)
  ([n]
   (remove-commented-parts n {} {}))

  ([n tags]
   (remove-commented-parts n tags {}))

  ([n tags options]
   (let [grey-zones (grey-zones.engine/grey-zones n tags options)]
        (letfn [; - If there is at least one commented zone left in the 'commented-zones' vector,
                ;   it uses the first commented zone's boundaries to cut out the first commented zone from the 'result' string
                ;   and calls itself recursivelly (while dropping the first zone from the 'commented-zones' vector).
                ; - If there is no more commented zone left in the 'commented-zones' vector it returns the result.
                ; - It uses the actual 'adjust' value to determine how many characters are already cut out from the result,
                ;   and to adjust the cut boundaries in every iteration.
                (f1 [result adjust [commented-zone _ :as commented-zones]]
                    (if commented-zone (let [updated-result (comments.utils/remove-commented-part result adjust commented-zone options)
                                             updated-adjust (+ adjust (seqable/count-difference result updated-result))]
                                            (f1 updated-result updated-adjust (vector/remove-first-item commented-zones)))
                                       (-> result)))]
               ; ...
               (f1 n 0 (:commented grey-zones))))))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn get-commented-parts
  ; @description
  ; Returns the commented parts of the given 'n' string.
  ;
  ; @param (string) n
  ; @param (vectors in vector)(opt) tags
  ; Default:
  ; [[:comment #"\;" #"\n" {:disable-interpreter? true}]
  ;  [:quote   #"\"" #"\"" {:disable-interpreter? true}]]
  ; @param (map)(opt) options
  ; {:endpoint (integer)(opt)
  ;   Quits collecting commented parts at the given 'endpoint' position in the given 'n' string.
  ;  :offset (integer)(opt)
  ;   Starts collecting commented parts from the given 'offset' position in the given 'n' string.}
  ;
  ; @usage
  ; (get-commented-parts "(defn my-function [])\n ; My comment\n")
  ;
  ; @example
  ; (get-commented-parts "(defn my-function [])\n ; My comment\n")
  ; =>
  ; ["; My comment\n"]
  ;
  ; @example
  ; (get-commented-parts "body { /* My comment */ color: blue; }"
  ;                      [[:comment #"/\*.*\*"]])
  ; =>
  ; ["/* My comment */"]
  ;
  ; @return (strings in vector)
  ([n]
   (get-commented-parts n {} {}))

  ([n tags]
   (get-commented-parts n tags {}))

  ([n tags options]
   (let [grey-zones (grey-zones.engine/grey-zones n tags options)]
        (letfn [(f0 [{:keys [started-at ended-at]}] (string/keep-range n started-at ended-at))]
               (-> grey-zones :commented (vector/->items f0))))))
