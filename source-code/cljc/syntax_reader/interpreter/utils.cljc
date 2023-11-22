
(ns syntax-reader.interpreter.utils
    (:require [map.api     :as map]
              [string.api  :as string]
              [regex.api   :as regex]
              [seqable.api :as seqable]
              [vector.api  :as vector]))

;; -- Ancestor / parent tag functions -----------------------------------------
;; ----------------------------------------------------------------------------

(defn no-tags-opened?
  ; @ignore
  ;
  ; @description
  ; Returns TRUE if there is no opened tag at the actual cursor position.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; @param (keyword) tag-name
  ;
  ; @return (boolean)
  [_ _ _ {:keys [actual-tags]} _]
  (letfn [(f [{:keys [closed-at opened-at opens-at]}]
             (or closed-at (not (or opens-at opened-at))))]
         (vector/all-items-match? actual-tags f)))

(defn tag-actual-depth
  ; @ignore
  ;
  ; @description
  ; Returns the actual opened depth of a specific tag.
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
                               (-> actual-tag :closed-at nil?)
                               (or (-> actual-tag :opens-at  integer?)
                                   (-> actual-tag :opened-at integer?))))]
         (vector/match-count actual-tags f)))

(defn ancestor-tags
  ; @ignore
  ;
  ; @description
  ; Returns the ancestor tags of the actual cursor position.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; {:actual-tags (maps in vector)}
  ;
  ; @return (maps in vector)
  [_ _ _ {:keys [actual-tags]}]
  (letfn [(f [{:keys [opened-at opens-at closed-at]}] (and (or opens-at opened-at) (not closed-at)))]
         (vector/keep-items-by actual-tags f)))

(defn parent-tag
  ; @ignore
  ;
  ; @description
  ; Returns the parent tag of the actual cursor position.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; {:actual-tags (maps in vector)}
  ;
  ; @return (map)
  [_ _ _ {:keys [actual-tags]}]
  (letfn [(f [{:keys [opened-at opens-at closed-at]}] (and (or opens-at opened-at) (not closed-at)))]
         (vector/last-match actual-tags f)))

(defn tag-ancestor?
  ; @ignore
  ;
  ; @description
  ; Returns TRUE the given tag is an opened ancestor tag of the actual cursor position.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; @param (keyword) tag-name
  ;
  ; @return (integer)
  [n tags options state tag-name]
  (< 0 (tag-actual-depth n tags options state tag-name)))

(defn tag-parent?
  ; @ignore
  ;
  ; @description
  ; Returns TRUE if the given tag is the opened parent tag of the actual cursor position.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; @param (keyword) tag-name
  ;
  ; @return (boolean)
  [n tags options state tag-name]
  (if-let [parent-tag (parent-tag n tags options state)]
          (-> parent-tag :name (= tag-name))))

;; -- Iteration functions -----------------------------------------------------
;; ----------------------------------------------------------------------------

(defn offset-reached?
  ; @ignore
  ;
  ; @description
  ; Returns TRUE if the actual cursor position reached the given 'offset' position.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; {:offset (integer)(opt)}
  ; @param (map) state
  ; {:cursor (integer)}
  ;
  ; @return (keyword)
  [n _ {:keys [offset] :or {offset 0}} {:keys [cursor]}]
  (let [offset (seqable/normalize-cursor n offset)]
       (>= cursor offset)))

(defn endpoint-reached?
  ; @ignore
  ;
  ; @description
  ; Returns TRUE if the actual cursor position reached the given 'endpoint' position.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; {:endpoint (integer)(opt)}
  ; @param (map) state
  ; {:cursor (integer)}
  ;
  ; @return (keyword)
  [n _ {:keys [endpoint] :or {endpoint (count n)}} {:keys [cursor]}]
  (let [endpoint (seqable/normalize-cursor n endpoint)]
       (>= cursor endpoint)))

(defn iteration-ended?
  ; @ignore
  ;
  ; @description
  ; Returns TRUE if the actual cursor position reached the last cursor position in the given 'n' string.
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

(defn iteration-stopped?
  ; @ignore
  ;
  ; @description
  ; Returns TRUE if the 'stop' metafunction stopped the iteration.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; {:cursor (integer)}
  ;
  ; @return (boolean)
  [_ _ _ {:keys [cursor]}]
  (= cursor :iteration-stopped))

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
  [n tags options {:keys [actual-tags] :as state}]
  (if-let [parent-tag (parent-tag n tags options state)]
          (if (-> tags (get-in [(:name parent-tag) 2 :disable-interpreter?]))
              (:name parent-tag))))

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

(defn reading-any-opening-tag?
  ; @ignore
  ;
  ; @description
  ; Returns TRUE if the last found opening tag is already started but not ended yet at the actual cursor position.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; {:actual-tags (maps in vector)}
  ;
  ; @return (boolean)
  [_ _ _ {:keys [actual-tags]}]
  (-> actual-tags last :will-open-at some?))

(defn reading-any-closing-tag?
  ; @ignore
  ;
  ; @description
  ; Returns TRUE if the last found closing tag is already started but not ended yet at the actual cursor position.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; {:actual-tags (maps in vector)}
  ;
  ; @return (boolean)
  [_ _ _ {:keys [actual-tags]}]
  (-> actual-tags last :will-end-at some?))

;; -- Tag processing requirement functions ------------------------------------
;; ----------------------------------------------------------------------------

(defn tag-requires-no-ancestors?
  ; @ignore
  ;
  ; @description
  ; Returns TRUE if the given tag requires no ancestor tags.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; @param (keyword) tag-name
  ;
  ; @return (boolean)
  [_ tags _ _ tag-name]
  (-> tags tag-name last :accepted-ancestors (= [])))

(defn tag-requires-no-parents?
  ; @ignore
  ;
  ; @description
  ; Returns TRUE if the given tag requires no parent tags.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; @param (keyword) tag-name
  ;
  ; @return (boolean)
  [_ tags _ _ tag-name]
  (-> tags tag-name last :accepted-parents (= [])))

(defn tag-requires-accepted-ancestor?
  ; @ignore
  ;
  ; @description
  ; Returns TRUE if the given tag requires any accepted ancestor tags.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; @param (keyword) tag-name
  ;
  ; @return (boolean)
  [_ tags _ _ tag-name]
  (-> tags tag-name last :accepted-ancestors vector/nonempty?))

(defn tag-requires-accepted-parent?
  ; @ignore
  ;
  ; @description
  ; Returns TRUE if the given tag requires any accepted ancestor tags.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; @param (keyword) tag-name
  ;
  ; @return (boolean)
  [_ tags _ _ tag-name]
  (-> tags tag-name last :accepted-parents vector/nonempty?))

(defn tag-any-accepted-ancestor-opened?
  ; @ignore
  ;
  ; @description
  ; Returns TRUE if at least one of the accepted ancestor tags of the given tag is opened.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; @param (keyword) tag-name
  ;
  ; @return (boolean)
  [n tags options state tag-name]
  (if-let [accepted-ancestors (-> tags tag-name last :accepted-ancestors)]
          (letfn [(f [accepted-ancestor] (tag-ancestor? n tags options state accepted-ancestor))]
                 (some f accepted-ancestors))))

(defn tag-any-accepted-parent-opened?
  ; @ignore
  ;
  ; @description
  ; Returns TRUE if at least one of the accepted parent tags of the given tag is opened (as the the actual parent tag).
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
          (letfn [(f [accepted-parent] (tag-parent? n tags options state accepted-parent))]
                 (some f accepted-parents))))

(defn tag-ancestor-requirements-met?
  ; @ignore
  ;
  ; @description
  ; Returns TRUE if the actual cursor position meets the given tag's ancestor requirements.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; @param (keyword) tag-name
  ;
  ; @return (boolean)
  [n tags options state tag-name]
  (and (or (-> (tag-requires-no-ancestors?        n tags options state tag-name) not)
           (-> (no-tags-opened?                   n tags options state)))
       (or (-> (tag-requires-accepted-ancestor?   n tags options state tag-name) not)
           (-> (tag-any-accepted-ancestor-opened? n tags options state tag-name)))))

(defn tag-parent-requirements-met?
  ; @ignore
  ;
  ; @description
  ; Returns TRUE if the actual cursor position meets the given tag's parent requirements.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; @param (keyword) tag-name
  ;
  ; @return (boolean)
  [n tags options state tag-name]
  (and (or (-> (tag-requires-no-parents?        n tags options state tag-name) not)
           (-> (no-tags-opened?                 n tags options state)))
       (or (-> (tag-requires-accepted-parent?   n tags options state tag-name) not)
           (-> (tag-any-accepted-parent-opened? n tags options state tag-name)))))

;; -- Regex functions ---------------------------------------------------------
;; ----------------------------------------------------------------------------

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

;; -- Update child / parent tag functions -------------------------------------
;; ----------------------------------------------------------------------------

(defn start-child-tag
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
  ; (start-child-tag "..." {...} {...}
  ;                  {:cursor 7 :actual-tags [{:name :paren :started-at 1 :opened-at 2}
  ;                                           {:name :paren :started-at 4 :opened-at 5}]}
  ;                  :paren)
  ; =>
  ; {:cursor 7 :actual-tags [{:name :paren :started-at 1 :opened-at 2}
  ;                          {:name :paren :started-at 4 :opened-at 5}
  ;                          {:name :paren :starts-at  7 :will-open-at 8}]}
  ;
  ; @return (map)
  [n tags options {:keys [cursor] :as state} tag-name]
  (let [tag-will-open-at (tag-will-open-at n tags options state tag-name)]
       (update state :actual-tags vector/conj-item {:name tag-name :starts-at cursor :will-open-at tag-will-open-at})))

(defn close-parent-tag
  ; @ignore
  ;
  ; @description
  ; Updates the given 'state' by closing the actual parent tag in the 'actual-tags' vector.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; {:actual-tags (maps in vector)
  ;  :cursor (integer)}
  ; @param (keyword) tag-name
  ;
  ; @example
  ; (close-parent-tag "..." {...} {...}
  ;                   {:cursor 10 :actual-tags [{:name :paren :started-at 1 :opened-at 2}
  ;                                             {:name :paren :started-at 4 :opened-at 5}
  ;                                             {:name :paren :started-at 7 :opened-at 8}]})
  ; =>
  ; {:cursor 10 :actual-tags [{:name :paren :started-at 1 :opened-at 2}
  ;                           {:name :paren :started-at 4 :opened-at 5}
  ;                           {:name :paren :started-at 7 :opened-at 8 :closes-at 10 :will-end-at 11}]}
  ;
  ; @return (map)
  [n tags options {:keys [actual-tags cursor] :as state} _]
  (let [parent-tag      (parent-tag n tags options state)
        parent-tag-dex  (vector/last-dex-of actual-tags parent-tag)
        tag-will-end-at (tag-will-end-at n tags options state (:name parent-tag))]
       (update state :actual-tags vector/update-nth-item parent-tag-dex merge {:closes-at cursor :will-end-at tag-will-end-at})))

;; -- State functions ---------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn filter-provided-state
  ; @ignore
  ;
  ; @description
  ; Removes the ':result' from the actual state, because the result is provided to the applied function in a separate parameter.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ;
  ; @return (map)
  [_ _ _ state]
  (dissoc state :result))

;; -- Actual state functions --------------------------------------------------
;; ----------------------------------------------------------------------------

(defn actualize-previous-tags
  ; @ignore
  ;
  ; @description
  ; ...
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; {:cursor (integer)}
  ;
  ; @return (map)
  [_ _ _ {:keys [cursor] :as state}]
  (letfn [(f [%] (cond-> % (-> % :will-open-at (=      cursor))  (map/rekey-item :will-open-at :opens-at)
                           (-> % :will-end-at  (=      cursor))  (map/rekey-item :will-end-at  :ends-at)
                           (-> % :starts-at    (= (dec cursor))) (map/rekey-item :starts-at    :started-at)
                           (-> % :opens-at     (= (dec cursor))) (map/rekey-item :opens-at     :opened-at)
                           (-> % :closes-at    (= (dec cursor))) (map/rekey-item :closes-at    :closed-at)))]
         (update state :actual-tags vector/->items f)))

(defn actualize-updated-tags
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
  [_ _ _ state]
  (update state :actual-tags vector/remove-items-by :ends-at))

(defn check-for-opening-tag
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
  ; @return (keyword)
  [n tags options state]
  (letfn [(f [tag-name] (and (opening-tag-starts?            n tags options state tag-name)
                             (tag-ancestor-requirements-met? n tags options state tag-name)
                             (tag-parent-requirements-met?   n tags options state tag-name)
                             (-> tag-name)))]
         (and (-> (interpreter-enabled?     n tags options state))
              (-> (reading-any-opening-tag? n tags options state) not)
              (-> (reading-any-closing-tag? n tags options state) not)
              (or (some (fn [[tag-name _]] (f tag-name)) (map/filter-values tags (fn [[_ _ {:keys [priority] :or {priority :default}}]] (= priority :high))))
                  (some (fn [[tag-name _]] (f tag-name)) (map/filter-values tags (fn [[_ _ {:keys [priority] :or {priority :default}}]] (= priority :default))))
                  (some (fn [[tag-name _]] (f tag-name)) (map/filter-values tags (fn [[_ _ {:keys [priority] :or {priority :default}}]] (= priority :low))))))))

(defn check-for-closing-tag
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
  ; @return (keyword)
  [n tags options state]
  (and (-> (reading-any-opening-tag? n tags options state) not)
       (-> (reading-any-closing-tag? n tags options state) not)
       (if-let [parent-tag (parent-tag n tags options state)]
               (and (closing-tag-starts? n tags options state (:name parent-tag))
                    (:name parent-tag)))))

(defn update-previous-state
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
  (let [state (actualize-previous-tags n tags options state)]
       (or (if-let [found-opening-tag (check-for-opening-tag n tags options state)]
                   (start-child-tag n tags options state found-opening-tag))
           (if-let [found-closing-tag (check-for-closing-tag n tags options state)]
                   (close-parent-tag n tags options state found-closing-tag))
           (-> state))))

(defn update-actual-state
  ; @ignore
  ;
  ; @description
  ; After the 'f' function is applied, it stores the updated result in the actual 'state' map.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; @param (*) updated-result
  ;
  ; @return (map)
  ; {:cursor (integer or keyword)
  ;  :result (*)}
  [n tags options state updated-result]
  (cond (-> updated-result vector? not)           (-> state (assoc :result (-> updated-result)))
        (-> updated-result first (= :$stop))      (-> state (assoc :result (-> updated-result last) :cursor :iteration-stopped))
        (-> updated-result first (= :$set-state)) (-> state (merge (-> updated-result second)))
        :else                                     (-> state (assoc :result (-> updated-result)))))

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
  (let [state (actualize-updated-tags n tags options state)]
       (-> state)))
