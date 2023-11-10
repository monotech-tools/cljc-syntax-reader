
(ns syntax.check
    (:require [string.api :as string]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn position-escaped?
  [n cursor]
  (and (-> cursor zero? not)
       (= "\\" (subs n (dec cursor) cursor))))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn position-commented?
  ; @description
  ; - Returns TRUE if the given cursor in the 'n' string is in a commented section.
  ; - Quoted comment tags might cause false output.
  ;
  ; @param (string) n
  ; @param (dex) cursor
  ; @param (string) comment-open-tag
  ; Default: ";"
  ; @param (string)(opt) comment-close-tag
  ; Default: "\n"
  ;
  ; @usage
  ; (position-commented? "(defn my-function [])\n ; My comment\n" 5)
  ;
  ; @example
  ; (position-commented? "(defn my-function [])\n ; My comment\n" 5)
  ; =>
  ; false
  ;
  ; @example
  ; (position-commented? "(defn my-function [])\n ; My comment\n" 25)
  ; =>
  ; true
  ;
  ; @return (boolean)
  ([n cursor]
   (position-commented? n cursor ";" "\n"))

  ([n cursor comment-open-tag]
   (position-commented? n cursor comment-open-tag "\n"))

  ([n cursor comment-open-tag comment-close-tag]
   (boolean (let [observed-part (string/part n 0 cursor)]
                 (if-let [last-open-pos (string/last-dex-of observed-part comment-open-tag)]
                         (-> n (string/part last-open-pos cursor)
                               (string/contains-part? comment-close-tag)
                               (not)))))))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn position-quoted?
  ; @description
  ; Returns TRUE if the given cursor in the 'n' string is in a quoted section.
  ;
  ; @param (string) n
  ; @param (dex) cursor
  ; @param (map)(opt) options
  ; {:comment-close-tag (string)(opt)
  ;   Default: "\n"
  ;  :comment-open-tag (string)(opt)
  ;   Default ";"
  ;  :ignore-comments? (boolean)(opt)
  ;   Default: false}
  ;
  ; @usage
  ; (position-quoted? "\"My quote\" My string" 3)
  ;
  ; @example
  ; (position-quoted? "\"My quote\" My string" 3)
  ; =>
  ; true
  ;
  ; @example
  ; (position-quoted? "\"My quote\" My string" 13)
  ; =>
  ; false
  ;
  ; @return (boolean)
  [n cursor])
