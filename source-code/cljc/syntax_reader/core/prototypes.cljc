
(ns syntax-reader.core.prototypes
    (:require [syntax-interpreter.api :as syntax-interpreter]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn tags-prototype
  ; @ignore
  ;
  ; @param (vectors in vector) tags
  ;
  ; @return (vectors in vector)
  [tags]
  (or tags [(:comment syntax-interpreter/CLJ-PATTERNS)
            (:quote   syntax-interpreter/CLJ-PATTERNS)]))
