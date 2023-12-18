
(ns syntax-reader.remove.utils
    (:require [fruits.seqable.api :as seqable]
              [fruits.string.api  :as string]))

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
  (let [ending-tag         (ending-tag)
        tag-started-at     (tag-started-at ending-tag)
        projected-position (- tag-started-at adjust)
        projected-cursor   (- cursor         adjust)]
       (as-> result % ; Removing the tag body ...
                      (string/cut-range % projected-position projected-cursor {:keep-inline-position? keep-indents?})
                      ; Removing leftover blank line (if any) ...
                      (cond-> % (and remove-leftover-blank-lines? (string/in-blank-line? % projected-position))
                                (string/remove-containing-line projected-position)))))

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
  (let [updated-result (-> result (cut-ending-tag state metafunctions options))
        updated-adjust (-> result (seqable/count-difference updated-result) (+ adjust))]
       (use-metadata {:adjust updated-adjust} updated-result)))
