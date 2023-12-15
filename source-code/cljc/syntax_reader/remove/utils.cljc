
(ns syntax-reader.remove.utils
    (:require [fruits.string.api :as string]
              [fruits.seqable.api :as seqable]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn cut-ending-tag
  ; @ignore
  ;
  ; @param (string) result
  ; @param (map) state
  ; @param (map) metafunctions
  ; @param (map) options
  ;
  ; @return (string)
  [result {{:keys [adjust] :or {adjust 0}} :metadata :keys [cursor]}
   {:keys [ending-tag tag-started-at]}
   {:keys [keep-indents? remove-leftover-blank-lines?]}]
  (let [ending-tag     (ending-tag)
        tag-started-at (tag-started-at ending-tag)]
       (as-> result % ; Removing the tag body ...
                      (string/cut-range % (- tag-started-at adjust)
                                          (- cursor         adjust)
                                          {:keep-inline-position? keep-indents?})
                      ; Removing leftover blank line (if any) ...
                      (cond-> % (and remove-leftover-blank-lines? (string/in-blank-line? % tag-started-at))
                                (string/remove-containing-line tag-started-at)))))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn remove-ending-tag?
  ; @ignore
  ;
  ; @param (string) result
  ; @param (map) state
  ; @param (map) metafunctions
  ; @param (map) options
  ;
  ; @return (boolean)
  [_ _ {:keys [ending-tag tag-options]} _]
  (if-let [ending-tag (ending-tag)]
          (-> (tag-options ending-tag) :keep? not)))

(defn remove-ending-tag
  ; @ignore
  ;
  ; @param (string) result
  ; @param (map) state
  ; @param (map) metafunctions
  ; @param (map) options
  ;
  ; @return (string)
  [result {{:keys [adjust] :or {adjust 0}} :metadata :as state} {:keys [use-metadata] :as metafunctions} options]
  (let [updated-result (cut-ending-tag result state metafunctions options)
        updated-adjust (+ adjust (seqable/count-difference result updated-result))]
       (use-metadata {:adjust updated-adjust} updated-result)))
