
(ns syntax-reader.check)

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
