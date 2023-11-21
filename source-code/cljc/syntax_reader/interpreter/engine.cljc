
(ns syntax-reader.interpreter.engine
    (:require [syntax-reader.interpreter.metafunctions :as interpreter.metafunctions]
              [syntax-reader.interpreter.utils         :as interpreter.utils]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn interpreter
  ; @description
  ; - Applies the given 'f' function at each cursor position of the given 'n' string.
  ; - Provides a state of the actual cursor position and a set of metafunctions for the applied function.
  ; - The provided state contains the 'actual-tags' vector that describes the opened tags at the actual cursor position.
  ; - Metafunctions that are available within the applied 'f' function:
  ;   - 'closing-tag-starts?'
  ;   - 'closing-tag-ends?'
  ;   - 'interpreter-disabled-by'
  ;   - 'interpreter-disabled?'
  ;   - 'interpreter-enabled?'
  ;   - 'jump'
  ;   - 'opening-tag-starts?'
  ;   - 'opening-tag-ends?'
  ;   - 'reading-closing-tag?'
  ;   - 'reading-opening-tag?'
  ;   - 'stop'
  ;   - 'parent-tag-name'
  ;   - 'tag-actual-depth'
  ;   - 'tag-not-opened?'
  ;   - 'tag-opened?'
  ;   - 'tag-opened-at'
  ;   - 'tag-started-at'
  ;
  ; @param (string) n
  ; @param (function) f
  ; Applied at each cursor position.
  ; Takes the output of the previous iteration (or the given 'initial' value) as first parameter.
  ; Takes the state of the actual cursor position as second parameter.
  ; Takes a map with metafunctions as third parameter.
  ; @param (*) initial
  ; @param (vectors in map)(opt) tags
  ; {:my-tag (vector)
  ;   [(regex pattern) opening-tag
  ;    (regex pattern) closing-tag
  ;    (map)(opt) options
  ;     {:accepted-parents (keywords in vector)(opt)
  ;       Only processes this kind of tag if at least one of the accepted parent tags is opened.
  ;      :disable-interpreter? (boolean)(opt)
  ;       Disables processing of other tags whithin the tag (for comments, quotes, etc.).}]}
  ; @param (map)(opt) options
  ; {:endpoint (integer)(opt)
  ;   Stops the interpreter at the given 'endpoint' position.
  ;  :ignore-escaped? (boolean)(opt)
  ;   TODO
  ;   Default: true
  ;  :offset (integer)(opt)
  ;   Starts applying the given 'f' function at the given 'offset' position.
  ;   In order to make accurate tag map, the interpreter starts processing at the 0th position even if the 'offset' value is not 0.
  ;  :tag-priority-order (keywords in vector)(opt)
  ;   List of high priority tags' names (in order of priority) that are processed with priority if more than one opening tag starts at a cursor position.}
  ;
  ; @usage
  ; (interpreter "My string" (fn [result state metafunctions]) nil)
  ;
  ; @usage
  ; (interpreter "My string" println nil)
  ;
  ; @example
  ; (let [my-text     "abc(def(ghi))"
  ;       my-function #(if (= 8 (:cursor %2) %2 %1))
  ;       my-initial  nil
  ;       my-tags     {:paren [#"\(" #"\)"]}]
  ;     (interpreter my-text my-function my-initial my-tags)
  ; =>
  ; {:actual-tags [{:name :paren :depth 1 :started-at 3 :opened-at 4}
  ;                {:name :paren :depth 2 :started-at 7 :opened-at 8}]
  ;  :cursor 8}
  ;
  ; @example
  ; (let [my-text     "<div>Hello World!</div>"
  ;       my-function #(if (= 8 (:cursor %2) %2 %1))
  ;       my-initial  nil
  ;       my-tags     {:div [#"<div>" #"</div>"]}]
  ;     (interpreter my-text my-function my-initial my-tags)
  ; =>
  ; {:actual-tags [{:name :div :depth 1 :started-at 0 :opened-at 5}]
  ;  :cursor 8}
  ;
  ; @return (*)
  ([n f initial]
   (interpreter n f initial {} {}))

  ([n f initial tags]
   (interpreter n f initial tags {}))

  ([n f initial tags options]
   (letfn [; @description
           ; Returns the 'metafunctions' map that is provided for the applied 'f' function at each cursor position.
           ;
           ; @param (map) state
           ;
           ; @return (map)
           ; {:closing-tag-ends? (function)
           ;  :closing-tag-starts? (function)
           ;  :interpreter-disabled-by (function)
           ;  :interpreter-disabled? (function)
           ;  :interpreter-enabled? (function)
           ;  :jump (function)
           ;  :opening-tag-ends? (function)
           ;  :opening-tag-starts? (function)
           ;  :parent-tag-name (function)
           ;  :reading-closing-tag? (function)
           ;  :reading-opening-tag? (function)
           ;  :stop (function)
           ;  :tag-actual-depth (function)
           ;  :tag-not-opened? (function)
           ;  :tag-opened? (function)
           ;  :tag-opened-at (function)
           ;  :tag-started-at (function)}
           (f0 [state]
               {:closing-tag-ends?       (interpreter.metafunctions/closing-tag-ends-f        n tags options state)
                :closing-tag-starts?     (interpreter.metafunctions/closing-tag-starts-f      n tags options state)
                :interpreter-disabled-by (interpreter.metafunctions/interpreter-disabled-by-f n tags options state)
                :interpreter-disabled?   (interpreter.metafunctions/interpreter-disabled-f    n tags options state)
                :interpreter-enabled?    (interpreter.metafunctions/interpreter-enabled-f     n tags options state)
                :jump                    (interpreter.metafunctions/jump-f                    n tags options state)
                :opening-tag-ends?       (interpreter.metafunctions/opening-tag-ends-f        n tags options state)
                :opening-tag-starts?     (interpreter.metafunctions/opening-tag-starts-f      n tags options state)
                :parent-tag-name         (interpreter.metafunctions/parent-tag-name-f         n tags options state)
                :reading-closing-tag?    (interpreter.metafunctions/reading-closing-tag-f     n tags options state)
                :reading-opening-tag?    (interpreter.metafunctions/reading-opening-tag-f     n tags options state)
                :stop                    (interpreter.metafunctions/stop-f                    n tags options state)
                :tag-actual-depth        (interpreter.metafunctions/tag-actual-depth-f        n tags options state)
                :tag-not-opened?         (interpreter.metafunctions/tag-not-opened-f          n tags options state)
                :tag-opened?             (interpreter.metafunctions/tag-opened-f              n tags options state)
                :tag-opened-at           (interpreter.metafunctions/tag-opened-at-f           n tags options state)
                :tag-started-at          (interpreter.metafunctions/tag-started-at-f          n tags options state)})]

          ; ...
          (let [initial-state {:actual-tags nil :cursor 0 :result initial}]
               (loop [{:keys [result] :as state} initial-state]
                     (let [actual-state           (interpreter.utils/update-actual-state   n tags options state)
                           provided-state         (interpreter.utils/filter-provided-state n tags options actual-state)
                           offset-reached?        (interpreter.utils/offset-reached?       n tags options actual-state)
                           applied-function       (fn [result state metafunctions] (if offset-reached? (f result state metafunctions) result))
                           provided-metafunctions (-> actual-state f0)
                           updated-result         (-> result (applied-function provided-state provided-metafunctions))
                           updated-state          (-> actual-state (assoc :result updated-result))]
                          (cond (interpreter.utils/endpoint-reached?    n tags options updated-state) (-> updated-result)
                                (interpreter.utils/interpreter-ended?   n tags options updated-state) (-> updated-result)
                                (interpreter.utils/interpreter-stopped? n tags options updated-state) (-> updated-result second)
                                :next-iteration (let [prepared-state (interpreter.utils/prepare-next-state n tags options updated-state)]
                                                     (recur (-> prepared-state (update :cursor inc)))))))))))
