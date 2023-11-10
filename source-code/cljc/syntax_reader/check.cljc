
(ns syntax-reader.check
    (:require [string.api          :as string]
              [syntax-reader.utils :as utils]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn position-escaped?
  ; @description
  ; Returns whether a specific cursor position in the given 'n' string is preceeded by an escape character.
  ;
  ; @param (string) n
  ; @param (integer) cursor
  ;
  ; @usage
  ; (position-escaped? "My string\n" 10)
  ;
  ; @example
  ; (position-escaped? "My string\n" 10)
  ; =>
  ; true
  ;
  ; @example
  ; (position-escaped? "My string\n" 9)
  ; =>
  ; false
  ;
  ; @return (boolean)
  [n cursor]
  (and (-> cursor zero? not)
       (= "\\" (subs n (dec cursor) cursor))))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn position-commented?
  ; @description
  ; Returns whether the given cursor position in the 'n' string falls within a commented zone.
  ;
  ; @param (string) n
  ; @param (dex) cursor
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
  ;   Default: true
  ;  :ignore-escaped? (boolean)(opt)
  ;   Default: true}
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
   (position-commented? n cursor {} {}))

  ([n cursor tags]
   (position-commented? n cursor tags {}))

  ([n cursor tags options]
   (let [tags (-> tags utils/default-comment-tags utils/default-quote-tags)])))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn position-quoted?
  ; @description
  ; Returns whether the given cursor position in the 'n' string falls within a quoted zone.
  ;
  ; @param (string) n
  ; @param (dex) cursor
  ; @param (map)(opt) tags
  ; {:comment-close-tag (string)(opt)
  ;   Default: "\n"
  ;  :comment-open-tag (string)(opt)
  ;   Default: ";"
  ;  :quote-close-tag (string)(opt)
  ;  :quote-open-tag (string)(opt)
  ;   Default: "\""}
  ; @param (map)(opt) options
  ; {:ignore-commented? (boolean)(opt)
  ;   Default: true
  ;  :ignore-escaped? (boolean)(opt)
  ;   Default: true}
  ;
  ; @usage
  ; (position-quoted? "(defn my-function [] \"My quote\")" 5)
  ;
  ; @example
  ; (position-quoted? "(defn my-function [] \"My quote\")" 5)
  ; =>
  ; false
  ;
  ; @example
  ; (position-quoted? "(defn my-function [] \"My quote\")" 25)
  ; =>
  ; true
  ;
  ; @return (boolean)
  ([n cursor]
   (position-quoted? n cursor {} {}))

  ([n cursor tags]
   (position-quoted? n cursor tags {}))

  ([n cursor tags options]
   (let [tags (-> tags utils/default-comment-tags utils/default-quote-tags)])))
