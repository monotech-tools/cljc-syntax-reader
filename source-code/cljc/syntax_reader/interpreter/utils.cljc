
(ns syntax-reader.interpreter.utils
    (:require [map.api     :as map]
              [string.api  :as string]
              [regex.api   :as regex]
              [seqable.api :as seqable]
              [vector.api  :as vector]))

;; -- Interpreter functions ---------------------------------------------------
;; ----------------------------------------------------------------------------

(defn interpreter-disabled-by
  ; @ignore
  ;
  ; @description
  ; Returns the disabling tag's name if the interpreter is disabled by an opened tag.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; {:actual-tags (maps in vector)}
  ;
  ; @return (keyword)
  [_ tags _ {:keys [actual-tags]}]
  (if-let [tag-name (-> actual-tags last :name)]
          (if (-> tags (get-in [tag-name 2 :disable-interpreter?]))
              (-> tag-name))))

(defn interpreter-disabled?
  ; @ignore
  ;
  ; @description
  ; Returns TRUE if the interpreter is disabled by an opened tag.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ;
  ; @return (boolean)
  [n tags options state]
  (-> (interpreter-disabled-by n tags options state) some?))

(defn interpreter-enabled?
  ; @ignore
  ;
  ; @description
  ; Returns TRUE if the interpreter is NOT disabled by an opened tag.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ;
  ; @return (boolean)
  [n tags options state]
  (-> (interpreter-disabled-by n tags options state) not))

(defn interpreter-ended?
  ; @ignore
  ;
  ; @description
  ; Returns TRUE if the actual cursor position exceeded the end of the given 'n' string.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; {:cursor (integer)}
  ;
  ; @return (boolean)
  [n _ _ {:keys [cursor]}]
  (seqable/cursor-last? n cursor))

(defn interpreter-stopped?
  ; @ignore
  ;
  ; @description
  ; Returns TRUE if the 'stop' metafunction stopped the iteration by ...
  ; ... wrapping the 'result' value within a vector,
  ; ... putting the ':$stop' marker into the wrapping vector as its first item.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; {:result (*)}
  ;
  ; @return (boolean)
  [_ _ _ {:keys [result]}]
  (and (-> result vector?)
       (-> result first (= :$stop))))

;; -- State functions ---------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn filter-provided-state
  ; @ignore
  ;
  ; @description
  ; Filters the given state for the keys that are presented in the provided state (provided for the applied 'f' function).
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ;
  ; @return (map)
  [_ _ _ state]
  (select-keys state [:actual-tags :cursor]))

;; -- Tag functions -----------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn innermost-tag
  ; @ignore
  ;
  ; @description
  ; Returns the innermost / last opened tag's actual state.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; {:actual-tags (maps in vector)}
  ;
  ; @return (map)
  [_ _ _ {:keys [actual-tags]}]
  (vector/last-item actual-tags))

(defn tag-actual-depth
  ; @ignore
  ;
  ; @description
  ; Returns the actual opened depth of the given tag.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; {:actual-tags (maps in vector)}
  ; @param (keyword) tag-name
  ;
  ; @return (integer)
  [_ _ _ {:keys [actual-tags]} tag-name]
  (letfn [(f [actual-tag] (and (-> actual-tag :name (= tag-name))
                               (-> actual-tag :opened-at integer?)
                               (-> actual-tag :closed-at nil?)))]
         (vector/match-count actual-tags f)))

(defn tag-opened?
  ; @ignore
  ;
  ; @description
  ; Returns whether the given tag is opened in any depth.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; @param (keyword) tag-name
  ;
  ; @return (integer)
  [_ _ _ state tag-name]
  (< 0 (tag-actual-depth state tag-name)))

(defn opening-tag-starts?
  ; @ignore
  ;
  ; @description
  ; Returns TRUE if the given tag's opening tag starts at the actual cursor position.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; {:cursor (integer)}
  ; @param (keyword) tag-name
  ;
  ; @return (boolean)
  [n tags _ {:keys [cursor]} tag-name]
  (if-let [opening-tag (-> tags tag-name first)]
          (regex/starts-at? n opening-tag cursor)))

(defn opening-tag-ends?
  ; @ignore
  ;
  ; @description
  ; Returns TRUE if the given tag's opening tag ends at the actual cursor position.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; {:cursor (integer)}
  ; @param (keyword) tag-name
  ;
  ; @return (boolean)
  [n tags _ {:keys [cursor]} tag-name]
  (if-let [opening-tag (-> tags tag-name first)]
          (regex/ends-at? n opening-tag cursor)))

(defn closing-tag-starts?
  ; @ignore
  ;
  ; @description
  ; Returns TRUE if the given tag's closing tag starts at the actual cursor position.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; {:cursor (integer)}
  ; @param (keyword) tag-name
  ;
  ; @return (boolean)
  [n tags _ {:keys [cursor]} tag-name]
  (if-let [closing-tag (-> tags tag-name second)]
          (regex/starts-at? n closing-tag cursor)))

(defn closing-tag-ends?
  ; @ignore
  ;
  ; @description
  ; Returns TRUE if the given tag's closing tag ends at the actual cursor position.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; {:cursor (integer)}
  ; @param (keyword) tag-name
  ;
  ; @return (boolean)
  [n tags _ {:keys [cursor]} tag-name]
  (if-let [closing-tag (-> tags tag-name second)]
          (regex/ends-at? n closing-tag cursor)))

(defn tag-opened-at
  ; @ignore
  ;
  ; @description
  ; Returns the cursor position where the given tag's innermost depth opened (where its opening tag ended).
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; {:actual-tags (maps in vector)}
  ; @param (keyword) tag-name
  ;
  ; @return (integer)
  [_ _ _ {:keys [actual-tags]} tag-name]
  (letfn [(f [actual-tag] (-> actual-tag :name (= tag-name)))]
         (if-let [innermost-tag (vector/last-match actual-tags f)]
                 (:opened-at innermost-tag))))

(defn tag-started-at
  ; @ignore
  ;
  ; @description
  ; Returns the cursor position where the given tag's opening tag started.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; {:actual-tags (maps in vector)}
  ; @param (keyword) tag-name
  ;
  ; @return (integer)
  [_ _ _ {:keys [actual-tags]} tag-name]
  (letfn [(f [actual-tag] (-> actual-tag :name (= tag-name)))]
         (if-let [innermost-tag (vector/last-match actual-tags f)]
                 (:started-at innermost-tag))))

(defn tag-will-open-at
  ; @ignore
  ;
  ; @description
  ; Returns the cursor position where the given tag's opening tag will end if it started
  ; at the actual cursor position (actual cursor position + tag's opening tag length).
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; {:cursor (integer)}
  ; @param (keyword) tag-name
  ;
  ; @return (integer)
  [n tags _ {:keys [cursor]} tag-name]
  (+ cursor (-> n (string/keep-range cursor)
                  (regex/re-first (-> tags tag-name first))
                  (count))))

(defn tag-will-end-at
  ; @ignore
  ;
  ; @description
  ; Returns the cursor position where the given tag's closing tag will end if it started
  ; at the actual cursor position (actual cursor position + tag's closing tag length).
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; {:cursor (integer)}
  ; @param (keyword) tag-name
  ;
  ; @return (integer)
  [n tags _ {:keys [cursor]} tag-name]
  (+ cursor (-> n (string/keep-range cursor)
                  (regex/re-first (-> tags tag-name second))
                  (count))))

(defn tag-not-requires-any-accepted-parent?
  ; @ignore
  ;
  ; @description
  ; Returns TRUE if the given tag does not require an accepted parent.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; @param (keyword) tag-name
  ;
  ; @return (boolean)
  [n tags options state tag-name]
  (-> tags tag-name last :accepted-parents empty?))

(defn tag-any-accepted-parent-opened?
  ; @ignore
  ;
  ; @description
  ; Returns TRUE if at least one of the accepted parents of the given tag is opened at the actual cursor position.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; @param (keyword) tag-name
  ;
  ; @return (boolean)
  [n tags options state tag-name]
  (if-let [accepted-parents (-> tags tag-name last :accepted-parents)]
          (letfn [(f [accepted-parent] (tag-opened? n tags options state accepted-parent))]
                 (some f accepted-parents))))

;; -- Actual tag functions ----------------------------------------------------
;; ----------------------------------------------------------------------------

(defn add-innermost-tag
  ; @ignore
  ;
  ; @description
  ; Updates the given 'state' by adding a new depth for the given tag to the 'actual-tags' vector.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; {:cursor (integer)}
  ; @param (keyword) tag-name
  ;
  ; @example
  ; (add-innermost-tag "..." {...} {...}
  ;                    {:cursor 7 :actual-tags [{:name :paren :depth 1 :started-at 1 :opened-at 2}
  ;                                             {:name :paren :depth 2 :started-at 4 :opened-at 5}]}
  ;                    :paren)
  ; =>
  ; {:cursor 7 :actual-tags [{:name :paren :depth 1 :started-at 1 :opened-at 2}
  ;                          {:name :paren :depth 2 :started-at 4 :opened-at 5}
  ;                          {:name :paren :depth 3 :started-at 7 :will-open-at 8}]}
  ;
  ; @return (map)
  [n tags options {:keys [cursor] :as state} tag-name]
  (let [tag-actual-depth  (tag-actual-depth n tags options state tag-name)
        tag-will-open-at  (tag-will-open-at n tags options state tag-name)
        tag-opening-depth (inc tag-actual-depth)]
       (update state :actual-tags vector/conj-item {:depth        tag-opening-depth
                                                    :name         tag-name
                                                    :started-at   cursor
                                                    :will-open-at tag-will-open-at})))

(defn close-innermost-tag
  ; @ignore
  ;
  ; @description
  ; Updates the given 'state' by closing the innermost tag in the 'actual-tags' vector.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; {:actual-tags (maps in vector)
  ;  :cursor (integer)}
  ;
  ; @example
  ; (close-innermost-tag "..." {...} {...}
  ;                      {:cursor 10 :actual-tags [{:name :paren :depth 1 :started-at 1 :opened-at 2}
  ;                                                {:name :paren :depth 2 :started-at 4 :opened-at 5}
  ;                                                {:name :paren :depth 3 :started-at 7 :opened-at 8}]})
  ; =>
  ; {:cursor 10 :actual-tags [{:name :paren :depth 1 :started-at 1 :opened-at 2}
  ;                           {:name :paren :depth 2 :started-at 4 :opened-at 5}
  ;                           {:name :paren :depth 3 :started-at 7 :opened-at 8 :closed-at 10 :will-end-at 11}]}
  ;
  ; @return (map)
  [n tags options {:keys [actual-tags cursor] :as state}]
  (let [tag-name        (-> actual-tags last :name)
        tag-will-end-at (tag-will-end-at n tags options state tag-name)]
       (update state :actual-tags vector/update-last-item merge {:closed-at   cursor
                                                                 :will-end-at tag-will-end-at})))

;; -- Actual state functions --------------------------------------------------
;; ----------------------------------------------------------------------------

(defn reading-opening-tag?
  ; @ignore
  ;
  ; @description
  ; Returns TRUE if the last found opening tag is already started but not ended yet at the actual cursor position
  ; (a tag is started but not opened if its opening tag not ended yet).
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; {:actual-tags (maps in vector)
  ;  :cursor (integer)}
  ;
  ; @return (boolean)
  [_ _ _ {:keys [actual-tags cursor]}]
  (if-let [innermost-tag-will-open-at (-> actual-tags last :will-open-at)]
          (> innermost-tag-will-open-at cursor)))

(defn reading-closing-tag?
  ; @ignore
  ;
  ; @description
  ; Returns TRUE if the last found closing tag is already started but not ended yet at the actual cursor position
  ; (a tag is closed but not ended if its closing tag not ended yet).
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; {:actual-tags (maps in vector)
  ;  :cursor (integer)}
  ;
  ; @return (boolean)
  [_ _ _ {:keys [actual-tags cursor]}]
  (if-let [innermost-tag-will-end-at (-> actual-tags last :will-end-at)]
          (> innermost-tag-will-end-at cursor)))

(defn actualize-innermost-tag
  ; @ignore
  ;
  ; @description
  ; If the last (innermost) tag in the 'actual-tags' vector ...
  ; ... opens at the actual cursor position, it renames the ':will-open-at' key to ':opened-at' in the tag's property map.
  ; ... ends at the actual cursor position, it removes the tag from the 'actual-tags' vector.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; {:actual-tags (maps in vector)
  ;  :cursor (integer)}
  ;
  ; @return (map)
  [_ _ _ {:keys [actual-tags cursor] :as state}]
  (cond-> state (-> actual-tags last :will-open-at (= cursor)) (update :actual-tags vector/update-last-item map/rekey-item :will-open-at :opened-at)
                (-> actual-tags last :will-end-at  (= cursor)) (update :actual-tags vector/remove-last-item)))

(defn check-for-opening-tag
  ; @ignore
  ;
  ; @description
  ; - Adds a new innermost tag to the 'actual-tags' vector if an opening tag starts at the actual cursor position.
  ; - At first it checks for tags that are presented also in the ':tag-priority-order' vector, then it checks for the rest
  ;   that are presented only in the given 'tags' map.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; {:tag-priority-order (keywords in vector)}
  ; @param (map) state
  ;
  ; @return (map)
  [n tags {:keys [tag-priority-order] :as options} state]
  (letfn [(f [tag-name] (and (opening-tag-starts? n tags options state tag-name)
                             (or (tag-not-requires-any-accepted-parent? n tags options state tag-name)
                                 (tag-any-accepted-parent-opened?       n tags options state tag-name))
                             (-> tag-name)))]
         (or (if-let [found-opening-tag (or (some (fn [tag-name]     (f tag-name)) tag-priority-order)
                                            (some (fn [[tag-name _]] (f tag-name)) tags))]
                     (add-innermost-tag n tags options state found-opening-tag))
             (-> state))))

(defn check-for-closing-tag
  ; @ignore
  ;
  ; @description
  ; Closes the innermost tag in the 'actual-tags' vector if its closing tag starts at the actual cursor position.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; {:actual-tags (maps in vector)}
  ;
  ; @return (map)
  [n tags options {:keys [actual-tags] :as state}]
  (or (if-let [innermost-tag (vector/last-item actual-tags)]
              (if (closing-tag-starts? n tags options state (:name innermost-tag))
                  (close-innermost-tag n tags options state)))
      (-> state)))

(defn update-actual-state
  ; @ignore
  ;
  ; @description
  ; - Actualizes the innermost tag in 'actual-tags' vector if it opens or ends at the actual cursor position.
  ; - If no opened tag disables the interpreter (checked after the actualization) ...
  ;   ... it searches for any opening tag that starts at the actual cursor position.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ;
  ; @return (map)
  [n tags options state]
  (let [state (actualize-innermost-tag n tags options state)]
       (cond (reading-opening-tag?  n tags options state) (-> state)
             (reading-closing-tag?  n tags options state) (-> state)
             (interpreter-disabled? n tags options state) (-> state)
             :return (check-for-opening-tag n tags options state))))

(defn prepare-next-state
  ; @ignore
  ;
  ; @description
  ; ...
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ;
  ; @return (map)
  [n tags options state]
  (cond (reading-opening-tag? n tags options state) (-> state)
        (reading-closing-tag? n tags options state) (-> state)
        :return (check-for-closing-tag n tags options state)))
