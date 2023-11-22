
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
  ;   - 'no-tags-opened?'
  ;   - 'tag-actual-depth'
  ;   - 'ancestor-tags'
  ;   - 'parent-tag'
  ;   - 'tag-ancestor?'
  ;   - 'tag-parent?'
  ;   - 'tag-opened?'
  ;   - 'tag-not-opened?'
  ;   - 'interpreter-disabled-by'
  ;   - 'interpreter-disabled?'
  ;   - 'interpreter-enabled?'
  ;   - 'reading-any-opening-tag?'
  ;   - 'reading-any-closing-tag?'
  ;   - 'stop'
  ;   - 'set-state'
  ;   - 'tag-starts?'
  ;   - 'tag-opens?'
  ;   - 'tag-closes?'
  ;   - 'tag-ends?'
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
  ;     {:accepted-ancestors (keywords in vector or empty vector)(opt)
  ;       Only processes the tag if at least one of the accepted ancestor tags is opened.
  ;       Leave this vector empty for tags that are processed only if they have no ancestor tags.
  ;      :accepted-parents (keywords in vector or empty vector)(opt)
  ;       Only processes the tag if at least one of the accepted parent tags is opened.
  ;       Leave this vector empty for tags that are processed only if they have no parent tags.
  ;      :disable-interpreter? (boolean)(opt)
  ;       Disables processing of other tags whithin the tag (e.g., comments, quotes).
  ;      :priority (keyword)(opt)
  ;       In case of more than one 'opening-tag' regex pattern starts at the same cursor position,
  ;       the interpreter acknowledges the first one with the highest priority.
  ;       :low, :default, :high
  ;       Default: :default}]}
  ; @param (map)(opt) options
  ; {:endpoint (integer)(opt)
  ;   Stops the interpreter at the given 'endpoint' position.
  ;  :ignore-escaped? (boolean)(opt)
  ;   TODO
  ;   Default: true
  ;  :offset (integer)(opt)
  ;   Starts applying the given 'f' function at the given 'offset' position.
  ;   In order to make accurate tag map, the interpreter starts processing at the 0th position even if the 'offset' value is not 0.}
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
  ; {:actual-tags [{:name :paren :started-at 3 :opened-at 4}
  ;                {:name :paren :started-at 7 :opened-at 8}]
  ;  :cursor 8}
  ;
  ; @example
  ; (let [my-text     "<div>Hello World!</div>"
  ;       my-function #(if (= 8 (:cursor %2) %2 %1))
  ;       my-initial  nil
  ;       my-tags     {:div [#"<div>" #"</div>"]}]
  ;     (interpreter my-text my-function my-initial my-tags)
  ; =>
  ; {:actual-tags [{:name :div :started-at 0 :opened-at 5}]
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
           ; {:ancestor-tags (function)
           ;  :interpreter-disabled-by (function)
           ;  :interpreter-disabled? (function)
           ;  :interpreter-enabled? (function)
           ;  :no-tags-opened? (function)
           ;  :parent-tag (function)
           ;  :reading-any-closing-tag? (function)
           ;  :reading-any-opening-tag? (function)
           ;  :set-state (function)
           ;  :stop (function)
           ;  :tag-actual-depth (function)
           ;  :tag-ancestor? (function)
           ;  :tag-closes? (function)
           ;  :tag-ends? (function)
           ;  :tag-not-opened? (function)
           ;  :tag-opened? (function)
           ;  :tag-opens? (function)
           ;  :tag-parent? (function)
           ;  :tag-starts? (function)}
           (f0 [state]
               {:ancestor-tags            (interpreter.metafunctions/ancestor-tags-f           n tags options state)
                :interpreter-disabled-by  (interpreter.metafunctions/interpreter-disabled-by-f n tags options state)
                :interpreter-disabled?    (interpreter.metafunctions/interpreter-disabled-f    n tags options state)
                :interpreter-enabled?     (interpreter.metafunctions/interpreter-enabled-f     n tags options state)
                :no-tags-opened?          (interpreter.metafunctions/no-tags-opened-f          n tags options state)
                :parent-tag               (interpreter.metafunctions/parent-tag-f              n tags options state)
                :reading-any-closing-tag? (interpreter.metafunctions/reading-any-closing-tag-f n tags options state)
                :reading-any-opening-tag? (interpreter.metafunctions/reading-any-opening-tag-f n tags options state)
                :set-state                (interpreter.metafunctions/set-state-f               n tags options state)
                :stop                     (interpreter.metafunctions/stop-f                    n tags options state)
                :tag-actual-depth         (interpreter.metafunctions/tag-actual-depth-f        n tags options state)
                :tag-ancestor?            (interpreter.metafunctions/tag-ancestor-f            n tags options state)
                :tag-closes?              (interpreter.metafunctions/tag-closes-f              n tags options state)
                :tag-ends?                (interpreter.metafunctions/tag-ends-f                n tags options state)
                :tag-not-opened?          (interpreter.metafunctions/tag-not-opened-f          n tags options state)
                :tag-opened?              (interpreter.metafunctions/tag-opened-f              n tags options state)
                :tag-opens?               (interpreter.metafunctions/tag-opens-f               n tags options state)
                :tag-parent?              (interpreter.metafunctions/tag-parent-f              n tags options state)
                :tag-starts?              (interpreter.metafunctions/tag-starts-f              n tags options state)})]

          ; ...
          (let [initial-state {:actual-tags nil :cursor 0 :result initial}]
               (loop [{:keys [result] :as state} initial-state]
                     (let [actual-state           (interpreter.utils/update-previous-state n tags options state)
                           provided-state         (interpreter.utils/filter-provided-state n tags options actual-state)
                           offset-reached?        (interpreter.utils/offset-reached?       n tags options actual-state)
                           applied-function       (fn [result state metafunctions] (if offset-reached? (f result state metafunctions) result))
                           provided-metafunctions (-> actual-state f0)
                           updated-result         (-> result (applied-function provided-state provided-metafunctions))
                           updated-state          (interpreter.utils/update-actual-state n tags options actual-state updated-result)]
                          (cond (interpreter.utils/iteration-stopped? n tags options updated-state) (-> updated-state :result)
                                (interpreter.utils/endpoint-reached?  n tags options updated-state) (-> updated-state :result)
                                (interpreter.utils/iteration-ended?   n tags options updated-state) (-> updated-state :result)
                                :next-iteration (let [prepared-state (interpreter.utils/prepare-next-state n tags options updated-state)]
                                                     (recur (-> prepared-state (update :cursor inc)))))))))))
