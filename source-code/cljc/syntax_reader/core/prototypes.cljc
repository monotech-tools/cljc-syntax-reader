
(ns syntax-reader.core.prototypes
    (:require [syntax-reader.core.config :as core.config]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn comment-tag-prototype
  ; @ignore
  ;
  ; @param (vector) comment-tag
  ; [(regex pattern) opening-pattern
  ;  (regex pattern) closing-pattern
  ;  (map)(opt) options]
  ;
  ; @example
  ; (comment-tag-prototype [#";" #"\n"])
  ; =>
  ; [#";" #"\n" {:disable-interpreter? true}]
  ;
  ; @example
  ; (comment-tag-prototype [])
  ; =>
  ; [#";" #"\n" {:disable-interpreter? true}]
  ;
  ; @return (vector)
  ; [(regex-pattern) opening-pattern
  ;  (regex-pattern) closing-pattern
  ;  (map) options]
  [[opening-pattern closing-pattern options]]
  [(or opening-pattern (-> core.config/DEFAULT-TAGS :comment first))
   (or closing-pattern (-> core.config/DEFAULT-TAGS :comment second))
   (merge options {:disable-interpreter? true})])

(defn quote-tag-prototype
  ; @ignore
  ;
  ; @param (vector) quote-tag
  ; [(regex pattern) opening-pattern
  ;  (regex pattern) closing-pattern
  ;  (map)(opt) options]
  ;
  ; @example
  ; (quote-tag-prototype [#"\"" #"\""])
  ; =>
  ; [#"\"" #"\"" {:disable-interpreter? true}]
  ;
  ; @example
  ; (quote-tag-prototype [])
  ; =>
  ; [#"\"" #"\"" {:disable-interpreter? true}]
  ;
  ; @return (vector)
  ; [(regex-pattern) opening-pattern
  ;  (regex-pattern) closing-pattern
  ;  (map) options]
  [[opening-pattern closing-pattern options]]
  [(or opening-pattern (-> core.config/DEFAULT-TAGS :quote first))
   (or closing-pattern (-> core.config/DEFAULT-TAGS :quote second))
   (merge options {:disable-interpreter? true})])

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
  (merge tags (if ignore-commented? {:comment (comment-tag-prototype comment)})
              (if ignore-quoted?    {:quote   (quote-tag-prototype   quote)})))
