
(ns syntax-reader.core.prototypes
    (:require [syntax-interpreter.api :as syntax-interpreter]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn tags-prototype
  ; @ignore
  ;
  ; @param (map) tags
  ; {:comment (vector)(opt)
  ;  :quote (vector)(opt)}
  ; @param (map) options
  ; {:ignore-commented? (boolean)(opt)
  ;   Default: true
  ;  :ignore-quoted? (boolean)(opt)
  ;   Default: true}
  ;
  ; @return (map)
  ; {:comment (vector)
  ;  :quote (vector)}
  [{:keys [comment quote] :as tags} {:keys [ignore-commented? ignore-quoted?] :or {ignore-commented? true ignore-quoted? true}}]
  (merge (if ignore-commented? {:comment (:comment syntax-interpreter/CLJ-PATTERNS)})
         (if ignore-quoted?    {:quote   (:string  syntax-interpreter/CLJ-PATTERNS)})
         (-> tags)))
