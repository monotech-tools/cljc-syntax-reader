
(ns syntax-reader.update.utils
    (:require [fruits.noop.api    :refer [return]]
              [fruits.seqable.api :as seqable]
              [fruits.string.api  :as string]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn apply-update-f
  ; @ignore
  ;
  ; @param (string) result
  ; @param (map) state
  ; @param (map) metafunctions
  ; @param (map) options
  ;
  ; @return (string)
  [result {{:keys [adjust] :or {adjust 0}} :metadata :keys [cursor]} {:keys [ending-tag tag-body tag-options tag-started-at]} _]
  (let [ending-tag     (ending-tag)
        tag-started-at (tag-started-at ending-tag)
        tag-body       (tag-body       ending-tag)]
       (str (string/keep-range result 0 (- tag-started-at adjust))
            (try ((-> ending-tag tag-options :update-f (or return)) tag-body)
                 (catch Exception e nil))
            (string/keep-range result (- cursor adjust)))))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn update-ending-tag?
  ; @ignore
  ;
  ; @param (string) result
  ; @param (map) state
  ; @param (map) metafunctions
  ; @param (map) options
  ;
  ; @return (boolean)
  [_ _ {:keys [ending-tag]} _]
  (ending-tag))

(defn update-ending-tag
  ; @ignore
  ;
  ; @param (string) result
  ; @param (map) state
  ; @param (map) metafunctions
  ; @param (map) options
  ;
  ; @return (string)
  [result {{:keys [adjust] :or {adjust 0}} :metadata :as state} {:keys [use-metadata] :as metafunctions} options]
  (let [updated-result (apply-update-f result state metafunctions options)
        updated-adjust (+ adjust (seqable/count-difference result updated-result))]
       (use-metadata {:adjust updated-adjust} updated-result)))
