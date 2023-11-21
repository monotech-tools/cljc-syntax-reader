
(ns syntax-reader.comments.utils
    (:require [string.api :as string]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn remove-commented-part
  ; @ignore
  ;
  ; @description
  ; ...
  ;
  ; @param (string) result
  ; @param (integer) adjust
  ; @param (map) commented-zone
  ; {:ended-at (integer)
  ;  :started-at (integer)}
  ; @param (map) options
  ; {:keep-indents? (boolean)(opt)
  ;  :remove-leftover-blank-lines? (boolean)(opt)}
  ;
  ; @return (string)
  [result adjust {:keys [started-at ended-at]} {:keys [keep-indents? remove-leftover-blank-lines?]}]
  (let [zone-start (-> started-at (- adjust))
        zone-end   (-> ended-at   (- adjust))]
       (as-> result % ; Removing the commented part ...
                      (string/cut-range % zone-start zone-end {:keep-inline-position? keep-indents?})
                      ; Removing leftover blank line (if any) ...
                      (cond-> % (and remove-leftover-blank-lines? (string/in-blank-line? % zone-start))
                                (string/remove-containing-line zone-start)))))
