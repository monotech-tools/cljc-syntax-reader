
(ns syntax-reader.interpreter
    (:require [regex.api           :as regex]
              [seqable.api         :as seqable]
              [string.api          :as string]
              [syntax-reader.check :as check]
              [vector.api          :as vector]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn interpreter
  ; @description
  ; - Applies the given 'f' function at each cursor position of the given 'n' string.
  ; - Provides the state of the actual cursor position and a map of metafunctions
  ;   (that could derive values from the actual state or could affect on the interpreter)
  ;   for the applied function.
  ; - List of metafunctions that are available within the interpreter:
  ;   - 'closing-tag-starts?'
  ;   - 'closing-tag-ends?'
  ;   - 'interpreter-disabled?'
  ;   - 'opening-tag-starts?'
  ;   - 'opening-tag-ends?'
  ;   - 'stop'
  ;   - 'tag-actual-depth'
  ;   - 'tag-opened?'
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
  ;    (map)(opt) tag-options
  ;     {:disable-interpreter? (boolean)(opt)
  ;       Default: false}]}
  ; @param (map)(opt) options
  ; {:endpoint (integer)(opt)
  ;   Stops the iteration at the endpoint position in the given 'n' string.
  ;  :ignore-escaped? (boolean)(opt)
  ;   TODO
  ;   Default: true
  ;  :offset (integer)(opt)
  ;   Starts the iteration at the offset position in the given 'n' string.
  ;   Using an 'offset' value might cause inaccurate position map!}
  ;
  ; @usage
  ; (defn my-function [result state functions] ...)
  ; (interpreter "My string" my-function nil)
  ;
  ; @usage
  ; (interpreter "My string" println nil)
  ;
  ; @example
  ; (defn my-function
  ;   [result state functions]
  ;   (if (= 16 (:cursor state)) state result))
  ;
  ; (interpreter "(-> my-value (= 420))" my-function nil)
  ; =>
  ; {:cursor 16
  ;  :tag-map [[:paren 1  1]
  ;            [:paren 2 14]]}
  ;
  ; @usage
  ; (defn my-function
  ;   [result state {:keys [stop tag-actual-depth] :as functions}]
  ;   (if (= 1 (tag-actual-depth :my-tag))
  ;       (stop result)))
  ;
  ; (interpreter "<div>Hello World!</div>" my-function nil {:my-tag [#"<div>" #"</div>"]})
  ;
  ; @return (*)
  ([n f initial]
   (interpreter n f initial {} {}))

  ([n f initial tags]
   (interpreter n f initial tags {}))

  ([n f initial tags {:keys [endpoint ignore-escaped? offset] :or {endpoint (count n) offset 0}}]
   (letfn [

           ;; -- Metafunctions -----------------------------------------------------------
           ;; ----------------------------------------------------------------------------

           ; @description
           ; Returns the 'interpreter-disabled?' metafunction.
           ;
           ; @param (map) state
           ;
           ; @return (function)
           (f87 [state]
                ; @description
                ; Returns whether the interpreter is disabled by any opened tag.
                ;
                ; @example
                ; (defn my-function
                ;   [result state {:keys [interpreter-disabled? stop] :as functions}]
                ;   (if (interpreter-disabled?)
                ;       (stop (:cursor state))))
                ;
                ; (let [n       "(abc(;def\n))"
                ;       f       my-function
                ;       initial nil
                ;       tags    {:comment [#";" #"\n" {:disable-interpreter? true}] :paren [#"\(" #"\)"]}]
                ;      (interpreter n f initial tags))
                ; =>
                ; 6 ; <- The 6th cursor position is the first position where the interpreter is disabled by the ':comment' tag.
                ;
                ; @return (integer)
                (fn [] (-> state f1 some?)))

           ; @description
           ; Returns the 'stop' metafunction.
           ;
           ; @param (map) state
           ;
           ; @return (function)
           (f88 [_]
                ; @description
                ; Stops the interpreter immediatelly that returns the parameter of the 'stop' function as interpreter output.
                ;
                ; @param (*) result
                ;
                ; @example
                ; (defn my-function
                ;   [result state {:keys [stop] :as functions}]
                ;   (let [result (assoc result :my-value "My value")]
                ;        (if (not= 4 (:cursor state))
                ;            result                          ; <- Lets the interpreter run to the next iteration and returns the result.
                ;            (stop "4th cursor position")))) ; <- Stops the interpreter at the actual cursor position and returns the result.
                ;
                ; (let [n       "My string"
                ;       f       my-function
                ;       initial nil]
                ;      (interpreter n f initial))
                ; =>
                ; "4th cursor position"
                ;
                ; @return (vector)
                ; [(namespaced keyword) stop-marker
                ;  (*) result]
                (fn [result] [::$stop result]))

           ; @description
           ; Returns the 'tag-actual-depth' metafunction.
           ;
           ; @param (map) state
           ;
           ; @return (function)
           (f89 [state]
                ; @description
                ; Returns the actual depth of the given tag at the actual cursor position.
                ;
                ; @param (keyword) tag-name
                ;
                ; @example
                ; (defn my-function
                ;   [result state {:keys [stop tag-actual-depth] :as functions}]
                ;   (if (= 6 (:cursor state))
                ;       (stop (tag-actual-depth :paren))))
                ;
                ; (let [n       "(abc(def))"
                ;       f       my-function
                ;       initial nil
                ;       tags    {:paren [#"\(" #"\)"]}]
                ;      (interpreter n f initial tags))
                ; =>
                ; 2
                ;
                ; @return (integer)
                (fn [tag-name] (f2 state tag-name)))

           ; @description
           ; Returns the 'tag-opened?' metafunction.
           ;
           ; @param (map) state
           ;
           ; @return (function)
           (f90 [state]
                ; @description
                ; Returns whether the given tag is opened at the actual cursor position in any depth.
                ;
                ; @param (keyword) tag-name
                ;
                ; @example
                ; (defn my-function
                ;   [result state {:keys [stop tag-opened?] :as functions}]
                ;   (if (= 6 (:cursor state))
                ;       (stop (tag-opened? :paren))))
                ;
                ; (let [n       "(abc(def))"
                ;       f       my-function
                ;       initial nil
                ;       tags    {:paren [#"\(" #"\)"]}]
                ;      (interpreter n f initial tags))
                ; =>
                ; true
                ;
                ; @return (boolean)
                (fn [tag-name] (< 0 (f2 state tag-name))))

           ; @description
           ; Returns the 'opening-tag-starts?' metafunction.
           ;
           ; @param (map) state
           ;
           ; @return (function)
           (f91 [state]
                ; @description
                ; Returns whether the given tag's opening tag starts at the actual cursor position.
                ;
                ; @param (keyword) tag-name
                ;
                ; @example
                ; (defn my-function
                ;   [result state {:keys [opening-tag-starts? stop] :as functions}]
                ;   (if (opening-tag-starts? :paren)
                ;       (stop (str "An opening ':paren' tag starts at cursor position: " (:cursor state) "."))))
                ;
                ; (let [n       "(abc(def))"
                ;       f       my-function
                ;       initial nil
                ;       tags    {:paren [#"\(" #"\)"]}]
                ;      (interpreter n f initial tags))
                ; =>
                ; "An opening ':paren' tag starts at cursor position: 0."
                ;
                ; @return (boolean)
                (fn [tag-name] (= tag-name (f3 state))))

           ; @description
           ; Returns the 'opening-tag-ends?' metafunction.
           ;
           ; @param (map) state
           ;
           ; @return (function)
           (f92 [state]
                ; @description
                ; Returns whether the given tag's opening tag ends at the actual cursor position.
                ;
                ; @param (keyword) tag-name
                ;
                ; @example
                ; (defn my-function
                ;   [result state {:keys [opening-tag-ends? stop] :as functions}]
                ;   (if (opening-tag-ends? :paren)
                ;       (stop (str "An opening ':paren' tag ends at cursor position: " (:cursor state) "."))))
                ;
                ; (let [n       "(abc(def))"
                ;       f       my-function
                ;       initial nil
                ;       tags    {:paren [#"\(" #"\)"]}]
                ;      (interpreter n f initial tags))
                ; =>
                ; "An opening ':paren' tag ends at cursor position: 1."
                ;
                ; @return (boolean)
                (fn [tag-name] (= tag-name (f4 state))))

           ; @description
           ; Returns the 'closing-tag-starts?' metafunction.
           ;
           ; @param (map) state
           ;
           ; @return (function)
           (f93 [state]
                ; @description
                ; Returns whether the given tag's closing tag starts at the actual cursor position.
                ;
                ; @param (keyword) tag-name
                ;
                ; @example
                ; (defn my-function
                ;   [result state {:keys [closing-tag-starts? stop] :as functions}]
                ;   (if (closing-tag-starts? :paren)
                ;       (stop (str "A closing ':paren' tag starts at cursor position: " (:cursor state) "."))))
                ;
                ; (let [n       "(abc(def))"
                ;       f       my-function
                ;       initial nil
                ;       tags    {:paren [#"\(" #"\)"]}]
                ;      (interpreter n f initial tags))
                ; =>
                ; "A closing ':paren' tag starts at cursor position: 8."
                ;
                ; @return (boolean)
                (fn [tag-name] (= tag-name (f5 state))))

           ; @description
           ; Returns the 'closing-tag-ends?' metafunction.
           ;
           ; @param (map) state
           ;
           ; @return (function)
           (f94 [state]
                ; @description
                ; Returns whether the given tag's closing tag ends at the actual cursor position.
                ;
                ; @param (keyword) tag-name
                ;
                ; @example
                ; (defn my-function
                ;   [result state {:keys [closing-tag-ends? stop] :as functions}]
                ;   (if (closing-tag-ends? :paren)
                ;       (stop (str "A closing ':paren' tag ends at cursor position: " (:cursor state) "."))))
                ;
                ; (let [n       "(abc(def))"
                ;       f       my-function
                ;       initial nil
                ;       tags    {:paren [#"\(" #"\)"]}]
                ;      (interpreter n f initial tags))
                ; =>
                ; "A closing ':paren' tag ends at cursor position: 9."
                ;
                ; @return (boolean)
                (fn [tag-name] (= tag-name (f6 state))))

           ;; ----------------------------------------------------------------------------
           ;; ----------------------------------------------------------------------------

           ; @description
           ; Returns the 'functions' map that contains the metafunctions and that is passed to the applied 'f' function at each cursor position.
           ;
           ; @param (map) state
           ;
           ; @return (map)
           ; {:closing-tag-ends? (function)
           ;  :closing-tag-starts? (function)
           ;  :opening-tag-ends? (function)
           ;  :opening-tag-starts? (function)
           ;  :stop (function)
           ;  :tag-actual-depth (function)
           ;  :tag-opened? (function)}
           (f0 [state]
               {:opening-tag-starts?   (f91 state) :opening-tag-ends? (f92 state)
                :closing-tag-starts?   (f93 state) :closing-tag-ends? (f94 state)
                :tag-actual-depth      (f89 state) :tag-opened?       (f90 state)
                :interpreter-disabled? (f87 state) :stop              (f88 state)})

           ; @description
           ; If the interpreter is disabled by an opened tag, it returns the disabling tag's name.
           ;
           ; @param (map) state
           ; {:tag-map (vectors in vector)}
           ;
           ; @example
           ; (let [n       "(abc(def ;ghi\n))"
           ;       f       (fn [_ _ _])
           ;       initial nil
           ;       tags    {:comment [#";" #"\n"] :paren [#"\(" #"\)"]}]
           ;      (interpreter n f initial tags))
           ;
           ; (f1 {:tag-map [[:paren   1  1]
           ;                [:paren   2  5]
           ;                [:comment 1 10]] ...}) ; <- A ':comment' tag opened at the 10th cursor position and it is currently disabling the interpreter.
           ; =>
           ; :comment
           ;
           ; @return (keyword)
           (f1 [{:keys [tag-map]}]
               (if-let [tag-name (-> tag-map last first)]
                       (if (get-in tags [tag-name 2 :disable-interpreter?])
                           (-> tag-name))))

           ; @description
           ; Returns the actual depth of the given tag.
           ;
           ; @param (map) state
           ; {:tag-map (vectors in vector)}
           ; @param (keyword) tag-name
           ;
           ; @example
           ; (let [n "(abc(def ;ghi\n))"
           ;       f (fn [_ _ _])]
           ;      (interpreter n f nil {:comment [#";" #"\n"] :paren [#"\(" #"\)"]}))
           ;
           ; (f2 {:tag-map [[:paren   1  1]
           ;                [:paren   2  5]
           ;                [:comment 1 10]] ...}
           ;     :paren)
           ; =>
           ; 2
           ;
           ; @return (integer)
           (f2 [{:keys [tag-map]} tag-name]
               (vector/match-count tag-map #(-> % first (= tag-name))))

           ; @description
           ; Returns the name of any opening tag that starts at the actual cursor position.
           ;
           ; @param (map) state
           ; {:cursor (integer)}
           ;
           ; @example
           ; (let [n "(abc(def ;ghi\n))"
           ;       f (fn [_ _ _])]
           ;      (interpreter n f nil))
           ;
           ; (f3 {:cursor 0 ...})
           ; =>
           ; :paren ; <- 0th cursor position: a ':paren' opening tag starts.
           ;
           ; @return (keyword)
           (f3 [{:keys [cursor]}]
               (some (fn [[tag-name [opening-tag _]]] (if (regex/starts-at? n opening-tag cursor) tag-name))
                     (-> tags)))

           ; @description
           ; Returns the name of any opening tag that ends at the actual cursor position.
           ;
           ; @param (map) state
           ; {:cursor (integer)}
           ;
           ; @example
           ; (let [n "(abc(def ;ghi\n))"
           ;       f (fn [_ _ _])]
           ;      (interpreter n f nil))
           ;
           ; (f4 {:cursor 1 ...})
           ; =>
           ; :paren ; <- 1st cursor position: a ':paren' opening tag ends.
           ;
           ; @return (keyword)
           (f4 [{:keys [cursor]}]
               (some (fn [[tag-name [opening-tag _]]] (if (regex/ends-at? n opening-tag cursor) tag-name))
                     (-> tags)))

           ; @description
           ; Returns the name of any closing tag that starts at the actual cursor position.
           ;
           ; @param (map) state
           ; {:cursor (integer)}
           ;
           ; @example
           ; (let [n "(abc(def ;ghi\n))"
           ;       f (fn [_ _ _])]
           ;      (interpreter n f nil))
           ;
           ; (f5 {:cursor 15})
           ; =>
           ; :paren ; <- 15th cursor position: a ':paren' closing tag starts.
           ;
           ; @return (keyword)
           (f5 [{:keys [cursor]}]
               (some (fn [[tag-name [_ closing-tag]]] (if (regex/starts-at? n closing-tag cursor) tag-name))
                     (-> tags)))

           ; @description
           ; Returns the name of any closing tag that ends at the actual cursor position.
           ;
           ; @param (map) state
           ; {:cursor (integer)}
           ;
           ; @example
           ; (let [n "(abc(def ;ghi\n))"
           ;       f (fn [_ _ _])]
           ;      (interpreter n f nil))
           ;
           ; (f6 {:cursor 16})
           ; =>
           ; :paren ; <- 16th cursor position: a ':paren' closing tag ends.
           ;
           ; @return (keyword)
           (f6 [{:keys [cursor]}]
               (some (fn [[tag-name [_ closing-tag]]] (if (regex/ends-at? n closing-tag cursor) tag-name))
                     (-> tags)))

           ; @description
           ; Updates the given 'state' if any ...
           ; ... opening tag ends at the actual cursor position.
           ; ... closing tag started at the previous cursor position.
           ;
           ; @param (map) state
           ; {:cursor (integer)
           ;  :tag-map (vectors in vector)}
           ;
           ; @usage
           ; (let [n "(abc(def ;ghi\n))"
           ;       f (fn [_ _ _])]
           ;      (interpreter n f nil))
           ;
           ; (f7 {:cursor 12 ; <- 12th cursor position: the interpreter is disabled since the 10th cursor position by the ':comment' tag.
           ;      :tag-map [[:paren   1  1]
           ;                [:paren   2  5]
           ;                [:comment 1 10]]})
           ;
           ; @return (map)
           (f7 [{:keys [cursor tag-map] :as state}]
               (or ; The very first condition must be whether the interpreter is currently disabled by any opened tag.
                   (if-let [interpreter-disabled-by (f1 state)]
                           ; If the interpreter is disabled by the last processed (not the last found) tag ...
                           (if-let [found-closing-tag (f5 (update state :cursor dec))]
                                   ; If a closing tag starts at the previous cursor position ...
                                   (if (= interpreter-disabled-by found-closing-tag)
                                       ; If the found closing tag corresponds to the opening tag that disabled the interpreter before ...
                                       ; ... it removes the last opened tag (the one that disabled the interpreter) from the tag map.
                                       (update state :tag-map vector/before-last-match #(-> % first (= found-closing-tag)) {:return? true})))
                           ; If the interpreter is not disabled ...
                           (if-let [found-opening-tag (f4 state)]
                                   ; If an opening tag ends at the actual cursor position ...
                                   ; ... it adds the new tag to the tag map.
                                   (let [depth (f2 state found-opening-tag)]
                                        (update state :tag-map vector/conj-item [found-opening-tag (inc depth) cursor]))
                                   ; If NO opening tag ends at the actual cursor position ...
                                   ; ... it searches for closing tags.
                                   (if-let [found-closing-tag (f5 (update state :cursor dec))]
                                           ; - If a closing tag started at the previous cursor position ...
                                           ;   ... it removes the just closed tag from the tag map.
                                           ; - Removes the following (the rest) tags also (in case of somehow they aren't closed yet).
                                           ; - Uses the '{:return? true}' setting for the 'vector/before-last-match' function to make sure
                                           ;   it doesn't empty the whole tag map if it doesn't contain any opened tag with the given tag name.
                                           ;   E.g., In Clojure, comments end with a newline character but not every newline character means that
                                           ;         there is any opened comment tag to close.
                                           (update state :tag-map vector/before-last-match #(-> % first (= found-closing-tag)) {:return? true}))))
                   ; If nothing changed it falls back to the given state.
                   (-> state)))]

          ; ...
          (let [initial-state {:cursor offset :result initial :tag-map nil}]
               (loop [{:keys [cursor result tag-map] :as state} initial-state]
                     (cond ; If there is no more cursor position in the given 'n' string, it returns the last output of the applied applied 'f' function ...
                           (seqable/cursor-out-of-bounds? n cursor)
                           (-> result)
                           ; At the 0th cursor position it is impossible to check ...
                           ; ... whether an opening tag ENDED at the actual cursor position.
                           ; ... whether a closing tag started at the PREVIOUS cursor position.
                           ; ... whether the interpreter is disabled by an OPENED tag.
                           ; + to check whether a closing tag started at the previous cursor position, the cursor value must be a positive
                           ;   integer (after decreasing it by one), otherwise the 'regex/starts-at?' / 'regex/ends-at?' function would search
                           ;   at the other end of the provided string (because of the negative cursor position).
                           (= cursor 0)
                           (recur (update state :cursor inc))
                           ; If the 'stop' metafunction stopped the iteration by wrapping the 'result' value in a vector,
                           ; and putting the '::$stop' marker into it as its first item ...
                           (-> result vector? (and (-> result first (= ::$stop))))
                           (-> result second)
                           :else
                           (let [updated-state      (-> state         (f7))
                                 provided-state     (-> updated-state (select-keys [:cursor :interpreter-disabled? :tag-map]))
                                 provided-functions (-> updated-state (f0))
                                 updated-result     (-> result        (f provided-state provided-functions))]
                                (cond ; If the cursor reached the given 'endpoint' position in the given 'n' string ...
                                      (= cursor endpoint)
                                      (-> result)
                                      ; Calls itself recursivelly ...
                                      :next-iteration (recur (-> updated-state (assoc  :result updated-result)
                                                                               (update :cursor inc)))))))))))
