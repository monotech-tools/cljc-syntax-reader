
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
  (or tags [(:comment       syntax-interpreter/CLJ-PATTERNS)
            (:meta-string   syntax-interpreter/CLJ-PATTERNS)
            (:regex-pattern syntax-interpreter/CLJ-PATTERNS)
            (:string        syntax-interpreter/CLJ-PATTERNS)]))
 
