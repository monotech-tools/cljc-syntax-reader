
(ns syntax-reader.update.engine
    (:require [syntax-interpreter.api     :as interpreter]
              [syntax-reader.update.utils :as update.utils]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn update-tags
  ; @description
  ; Updates the given tags in the given 'n' value with the provided ':update-f' function.
  ;
  ; @param (string) n
  ; @param (vectors in vector)(in decreasing priority order) tags
  ; For more information, check out the documentation of the [syntax-interpreter.api/interpreter](https://mt-devtools.github.io/cljc-syntax-interpreter) function.
  ; [[(keyword) tag-name
  ;   (regex pattern) pattern / opening-pattern
  ;   (regex pattern)(opt) closing-pattern
  ;   (map)(opt) tag-options
  ;    {:update-f (function)(opt)
  ;      Default: return}]]
  ; @param (map)(opt) options
  ; For more information, check out the documentation of the [syntax-interpreter.api/interpreter](https://mt-devtools.github.io/cljc-syntax-interpreter) function.
  ;
  ; @usage
  ; (update-tags "abcdef/*ghijkl*/mnopqrs" [[:my-tag #"\/\*" #"\*\/" {:update-f clojure.string/upper-case}]])
  ; =>
  ; "abcdef/*GHIJKL*/mnopqrs"
  ;
  ; @return (string)
  ([n tags]
   (update-tags n tags {}))

  ([n tags options]
   (letfn [(f0 [result state metafunctions]
               (if (update.utils/update-ending-tag? result state metafunctions options)
                   (update.utils/update-ending-tag  result state metafunctions options)
                   (-> result)))]
          (interpreter/interpreter n f0 n tags options))))
