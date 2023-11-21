
(ns syntax-reader.search.utils
    (:require [regex.api :as regex]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn pattern-found?
  ; @ignore
  ;
  ; @param (string) n
  ; @param (regex pattern) x
  ; @param (map) state
  ; {:cursor (integer)}
  ; @param (map) metafunctions
  ; {:interpreter-enabled? (function)}
  ;
  ; @return (boolean)
  [n x {:keys [cursor]} {:keys [interpreter-enabled?]}]
  (and (interpreter-enabled?)
       (regex/starts-at? n x cursor)))
