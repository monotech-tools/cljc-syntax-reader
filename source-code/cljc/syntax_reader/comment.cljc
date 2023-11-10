
(ns syntax-reader.comment
    (:require [string.api  :as string]))
              ;[syntax.tags :as tags]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn remove-commented-zones
  ; @description
  ; Removes the commented zones from the given 'n' string.
  ;
  ; @param (string) n
  ; @param (map)(opt) tags
  ; {:comment-close-tag (string)(opt)
  ;   Default: "\n"
  ;  :comment-open-tag (string)(opt)
  ;   Default: ";"
  ;  :quote-close-tag (string)(opt)
  ;  :quote-open-tag (string)(opt)
  ;   Default: "\""}
  ; @param (map)(opt) options
  ; {:ignore-quotes? (boolean)(opt)
  ;   Default: true
  ;  :ignore-escaped? (boolean)(opt)
  ;   Default: true}
  ;
  ; @usage
  ; (remove-commented-zones "(defn my-function [])\n ; My comment\n")
  ;
  ; @example
  ; (remove-commented-zones "(defn my-function [])\n ; My comment\n")
  ; =>
  ; "(defn my-function [])\n "
  ;
  ; @example
  ; (remove-commented-zones "body { /* My comment */ color: blue; }"
  ;                         {:comment-open-tag "/*"
  ;                          :comment-close-tag "*/"})
  ; =>
  ; "body {  color: blue; }"
  ;
  ; @return (string)
  ([n]
   (remove-commented-zones n {} {}))

  ([n tags]
   (remove-commented-zones n tags {}))

  ([n tags options]))
   ; BUG#1130
   ; The Shadow CLJS protects the '+' function from receiving nil as a parameter.
   ; The 'close-tag-position' function returns through an if-let condition, therefore
   ; without using the (if (number? comment-close-pos) ...) condition, the Shadow CLJS
   ; would throw the following error message:
   ; "cljs.core/+, all arguments must be numbers, got [#{nil clj-nil} number] instead"

   ;(letfn [; - If both an opening and a closing tag is found in the given 'n' string, it removes
           ;   the part between the opening and closing tags.
           ; - If only an opening tag is found in the given 'n' string, it removes the part from
           ;   the opening tag.
    ;       (f0 [n] (if-let [comment-open-pos (tags/open-tag-position n comment-open-tag)]
    ;                       (if-let [comment-close-pos (tags/close-tag-position n comment-open-tag comment-close-tag)]
    ;                               (if (number? comment-close-pos)
    ;                                   (string/cut n comment-open-pos (+ comment-close-pos (count comment-close-tag)))
    ;                               (string/part n 0 comment-open-pos)

           ; Removes the first comment by applying the 'f0' function.
           ; If the 'f0' function has found anything to cut out, it returns the chunked
           ; result (otherwise it returns nil), after then the 'f1' function calls itself
           ; recursively, otherwise the 'f1' function returns the result from the previous iteration.
    ;       (f1 [n] (if-let [n (f0 n)]
    ;                       (-> n f1)
    ;                       (-> n)

          ; ...
    ;      (f1 n)]))
