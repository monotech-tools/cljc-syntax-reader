
(ns syntax-reader.interpreter.core
    (:require [regex.api                :as regex]
              [seqable.api              :as seqable]
              [string.api               :as string]
              [syntax-reader.core.check :as core.check]
              [syntax-reader.interpreter.metafunctions :as interpreter.metafunctions]
              [syntax-reader.interpreter.utils         :as interpreter.utils]
              [vector.api               :as vector]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn interpreter
  ; @description
  ; - Applies the given 'f' function at each cursor position of the given 'n' string.
  ; - Provides the state of the actual cursor position and a map with metafunctions for the applied function.
  ; - List of metafunctions that are available within the applied 'f' function:
  ;   - 'closing-tag-starts?'
  ;   - 'closing-tag-ends?'
  ;   - 'interpreter-disabled?'
  ;   - 'opening-tag-starts?'
  ;   - 'opening-tag-ends?'
  ;   - 'stop'
  ;   - 'tag-actual-depth'
  ;   - 'tag-not-opened?'
  ;   - 'tag-opened?'
  ;   - 'tag-opened-at'
  ;   - 'tag-started-at'
  ; - Provides the 'actual-tags' vector within the actual state for the applied 'f' function at each cursor position.
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
  ;       Disables processing of other tags whithin the tag.}]}
  ; @param (map)(opt) options
  ; {:endpoint (integer)(opt)
  ;   Stops the iteration at the endpoint position in the given 'n' string.
  ;  :ignore-escaped? (boolean)(opt)
  ;   TODO
  ;   Default: true
  ;  :offset (integer)(opt)
  ;   Starts the iteration at the offset position in the given 'n' string.
  ;   Using an 'offset' value might cause inaccurate position map!
  ;  :tag-priority-order (keywords in vector)(opt)
  ;   List of high priority tags' names (in order of priority.}
  ;
  ; @usage
  ; (defn my-function [result state metafunctions] ...)
  ; (interpreter "My string" my-function nil)
  ;
  ; @usage
  ; (interpreter "My string" println nil)
  ;
  ; @example
  ; (defn my-function
  ;   [result state metafunctions]
  ;   (if (= 16 (:cursor state)) state result))
  ;
  ; (interpreter "(-> my-value (= 420))" my-function nil)
  ; =>
  ; {:actual-tags [{:name :paren :depth 1 :started-at 0  :opened-at  2}
  ;                {:name :paren :depth 2 :started-at 13 :opened-at 14}]
  ;  :cursor 16}
  ;
  ; @usage
  ; (defn my-function
  ;   [result state {:keys [stop tag-actual-depth] :as metafunctions}]
  ;   (if (= 1 (tag-actual-depth :my-tag))
  ;       (stop result)))
  ;
  ; (interpreter "<div>Hello World!</div>" my-function nil {:div [#"<div>" #"</div>"]})
  ;
  ; @return (*)
  ([n f initial]
   (interpreter n f initial {} {}))

  ([n f initial tags]
   (interpreter n f initial tags {}))

  ([n f initial tags {:keys [endpoint ignore-escaped? offset tag-priority-order]
                      :or   {offset 0}}]
   (letfn [; @description
           ; Returns the 'metafunctions' map that is provided to the applied 'f' function at each cursor position.
           ;
           ; @param (map) state
           ;
           ; @return (map)
           ; {:closing-tag-ends? (function)
           ;  :closing-tag-starts? (function)
           ;  :interpreter-disabled? (function)
           ;  :opening-tag-ends? (function)
           ;  :opening-tag-starts? (function)
           ;  :stop (function)
           ;  :tag-actual-depth (function)
           ;  :tag-not-opened? (function)
           ;  :tag-opened? (function)
           ;  :tag-opened-at (function)
           ;  :tag-started-at (function)}
           (f0 [state]
               {:closing-tag-ends?     (interpreter.metafunctions/closing-tag-ends-f     n tags options state)
                :closing-tag-starts?   (interpreter.metafunctions/closing-tag-starts-f   n tags options state)
                :interpreter-disabled? (interpreter.metafunctions/interpreter-disabled-f n tags options state)
                :interpreter-enabled?  (interpreter.metafunctions/interpreter-enabled-f  n tags options state)
                :opening-tag-ends?     (interpreter.metafunctions/opening-tag-ends-f     n tags options state)
                :opening-tag-starts?   (interpreter.metafunctions/opening-tag-starts-f   n tags options state)
                :stop                  (interpreter.metafunctions/stop-f                 n tags options state)
                :tag-actual-depth      (interpreter.metafunctions/tag-actual-depth-f     n tags options state)
                :tag-not-opened?       (interpreter.metafunctions/tag-not-opened-f       n tags options state)
                :tag-opened?           (interpreter.metafunctions/tag-opened-f           n tags options state)
                :tag-opened-at         (interpreter.metafunctions/tag-opened-at-f        n tags options state)
                :tag-started-at        (interpreter.metafunctions/tag-started-at         n tags options state)})

           (f1 [{:keys [actual-tags cursor] :as state}])


           (f15 [{:keys [cursor actual-tags] :as state


                    ; - If the interpreter is disabled ...
                    ;   ... it doesn't process tag openings and closings.
                    ;   ... only cares about whether the disabling tag closes.
                    ; - If the disabling tag's closing tag started at the previous cursor position ...
                    ;   ... it closes the disabling tag in the 'actual-tags' vector.
                    ;   ... the interpreter will not be disabled in the next iteration.
                    (if-let [interpreter-disabled-by (f1 state)]
                            (cond-> state (f10 state interpreter-disabled-by) (f14 interpreter-disabled-by)))
                    ; If the 'tag-priority-order' vector is provided it iterates over that vector and checks
                    ; whether any tag's opening tag ends at the actual cursor position.
                    (if-let [found-opening-tag (some (fn [tag-name] (if (f7 state tag-name) tag-name)) tag-priority-order)])
                    (if (or (tag-has-no-required-parent? n tags options state tag-name)
                            (tag-required-parent-opened? n tags options state tag-name)
                            (f13 state found-opening-tag)))
                    ; Iterates over the given 'tags' map and checks whether any tag's opening tag ends at the actual cursor position.
                    (if-let [found-opening-tag (some (fn [[tag-name _]] (if (f7 state tag-name) tag-name)) tags)])
                    (if (or (tag-has-no-required-parent? n tags options state tag-name)
                            (tag-required-parent-opened? n tags options state tag-name)
                            (f13 state found-opening-tag)))
                    ; Iterates over the 'actual-tags' vector backwards (from innermost to outermost opened tag)
                    ; and checks whether any opened tag's closing tag started at the previous cursor position ...
                    ; ... removes the innermost depth of the found tag from the 'actual-tags' vector.
                    (if-let [found-closing-tag (:name (vector/last-match actual-tags (fn [{:keys [tag]}] (f10 state tag))))]
                            (f14 state found-closing-tag))
                    ; ...
                    (-> state)}])]

          ; ...
          (let [initial-state {:actual-tags nil :cursor offset :result initial}]
               (loop [{:keys [result] :as state} initial-state]
                     (let [actual-state   (interpreter.utils/update-actual-state   n tags options state)
                           provided-state (interpreter.utils/filter-provided-state n tags options actual-state)
                           provided-metafunctions (-> actual-state f0)
                           updated-result         (-> result (f provided-state provided-metafunctions))
                           updated-state          (-> actual-state (assoc :result updated-result))]
                          (cond (interpreter.utils/endpoint-reached?    n tags options actual-state)  (-> updated-result)
                                (interpreter.utils/interpreter-ended?   n tags options actual-state)  (-> updated-result)
                                (interpreter.utils/interpreter-stopped? n tags options updated-state) (-> updated-result second)
                                :next-iteration (let [prepared-state (interpreter.utils/prepare-next-state n tags options updated-state)]
                                                     (recur (-> prepared-state (update :cursor inc)))))))))))
