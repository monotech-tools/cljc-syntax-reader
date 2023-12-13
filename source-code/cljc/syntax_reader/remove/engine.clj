
(ns syntax-reader.remove.engine
    (:require [syntax-interpreter.api :as interpreter]
              [syntax-reader.remove.utils :as remove.utils]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn remove-tags
  ; @description
  ; Removes the given tags from the given 'n' value.
  ;
  ; @param (string) n
  ; @param (vectors in vector)(in decreasing priority order) tags
  ; For more information, check out the documentation of the 'syntax-interpreter.api/interpreter' function.
  ; [[(keyword) tag-name
  ;   (regex pattern) pattern / opening-pattern
  ;   (regex pattern)(opt) closing-pattern
  ;   (map)(opt) tag-options
  ;    {:keep? (boolean)(opt)
  ;      If TRUE, it keeps track of the tag but doesn't remove it.
  ;      Default: false}]]
  ; @param (map)(opt) options
  ; For more information, check out the documentation of the 'syntax-interpreter.api/interpreter' function.
  ;
  ; @example
  ; (remove-tags "abcdef/*ghijkl*/mnopqrs" [[:my-tag #"\/\*" #"\*\/"]])
  ; =>
  ; "abcdefmnopqrs"
  ;
  ; @return (string)
  ([n tags]
   (remove-tags n tags {}))

  ([n tags options]
   (letfn [(f0 [result state {:keys [ending-tag use-metadata] :as metafunctions}]
               (println (dissoc state :left-tags))
               (or (if-let [ending-tag (ending-tag)]
                           (if (remove.utils/remove-found-tag? result state metafunctions ending-tag)
                               (use-metadata (remove.utils/update-metadata  result state metafunctions ending-tag)
                                             (remove.utils/remove-found-tag result state metafunctions ending-tag))))
                   (-> result)))]
          (interpreter/interpreter n f0 n tags options))))
