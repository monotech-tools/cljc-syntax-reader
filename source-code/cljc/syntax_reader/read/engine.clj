
(ns syntax-reader.read.engine
    (:require [syntax-interpreter.api :as interpreter]
              [syntax-reader.read.utils :as read.utils]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn read-tags
  ; @description
  ; Reads the given tags from the given 'n' value.
  ;
  ; @param (string) n
  ; @param (vectors in vector)(in decreasing priority order) tags
  ; For more information, check out the documentation of the 'syntax-interpreter.api/interpreter' function.
  ; [[(keyword) tag-name
  ;   (regex pattern) pattern / opening-pattern
  ;   (regex pattern)(opt) closing-pattern
  ;   (map)(opt) tag-options]]
  ; @param (map)(opt) options
  ; For more information, check out the documentation of the 'syntax-interpreter.api/interpreter' function.
  ;
  ; @example
  ; (read-tags "abcdef/*ghijkl*/mnopqrs" [[:my-tag #"\/\*" #"\*\/"]])
  ; =>
  ; {:my-tag ["/*ghijkl*/"]}
  ;
  ; @return (map)
  ([n tags]
   (read-tags n tags {}))

  ([n tags options]
   (letfn [(f0 [result state {:keys [ending-tag] :as metafunctions}]
               (if-let [ending-tag (ending-tag)]
                       (read.utils/read-found-tag result state metafunctions ending-tag)
                       (-> result)))]
          (interpreter/interpreter n f0 {} tags options))))
