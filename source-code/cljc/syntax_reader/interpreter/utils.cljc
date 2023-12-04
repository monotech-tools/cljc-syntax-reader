
(ns syntax-reader.interpreter.utils
    (:require [map.api     :as map :refer [update-by]]
              [string.api  :as string]
              [regex.api   :as regex]
              [seqable.api :as seqable :refer [last-dex]]
              [syntax-reader.core.config :as core.config]
              [vector.api  :as vector]))

;; -- Tag parameter functions -------------------------------------------------
;; ----------------------------------------------------------------------------

(defn tag-opening-pattern
  ; @ignore
  ;
  ; @description
  ; Returns the given tag's opening pattern.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; @param (keyword) tag-name
  ;
  ; @return (regex pattern)
  [_ tags _ _ tag-name]
  (if-let [opening-pattern (-> tags tag-name first)]
          (if (regex/pattern? opening-pattern) opening-pattern)))

(defn tag-closing-pattern
  ; @ignore
  ;
  ; @description
  ; Returns the given tag's closing pattern (if provided).
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; @param (keyword) tag-name
  ;
  ; @return (regex pattern)
  [_ tags _ _ tag-name]
  (if-let [closing-pattern (-> tags tag-name second)]
          (if (regex/pattern? closing-pattern) closing-pattern)))

(defn tag-options
  ; @ignore
  ;
  ; @description
  ; Returns the given tag's options map (if provided).
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; @param (keyword) tag-name
  ;
  ; @return (map)
  [_ tags _ _ tag-name]
  (if-let [tag-options (-> tags tag-name last)]
          (if (map? tag-options) tag-options)))

(defn tag-omittag?
  ; @ignore
  ;
  ; @description
  ; Returns TRUE if the given tag doesn't have a closing pattern (omittag).
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; @param (keyword) tag-name
  ;
  ; @return (map)
  [n tags options state tag-name]
  (-> (tag-closing-pattern n tags options state tag-name) nil?))

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
  ;
  ; @return (boolean)
  [_ _ _ {:keys [actual-tags]}]
  (letfn [(f [{:keys [closed-at opened-at opens-at]}]
             ; Tags that are either already closed or not opened yet:
             (or closed-at (not (or opens-at opened-at))))]
         (vector/all-items-match? actual-tags f)))

(defn depth
  ; @ignore
  ;
  ; @description
  ; Returns the depth of the actual cursor position.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; {:actual-tags (maps in vector)}
  ;
  ; @return (integer)
  [_ _ _ {:keys [actual-tags]}]
  (letfn [(f [{:keys [closed-at opened-at opens-at]}]
             ; Tags that are already opened and aren't closed yet:
             (and (or opens-at opened-at) (not closed-at)))]
         (vector/match-count actual-tags f)))

(defn tag-depth
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
  (letfn [(f [{:keys [closed-at name opened-at opens-at]}]
             ; Tags with a specific tag name that are already opened and aren't closed yet:
             (and (= name tag-name) (or opens-at opened-at) (not closed-at)))]
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
  (letfn [(f [{:keys [opened-at opens-at closed-at]}]
             ; Tags that are already opened and aren't closed yet:
             (and (or opens-at opened-at) (not closed-at)))]
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
  (letfn [(f [{:keys [opened-at opens-at closed-at]}]
             ; Tags that are already opened and aren't closed yet:
             (and (or opens-at opened-at) (not closed-at)))]
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
  (< 0 (tag-depth n tags options state tag-name)))

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

(defn left-sibling-count
  ; @ignore
  ;
  ; @description
  ; Returns how many siblings have been already left behind by the interpreter within the actual parent tag.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ;
  ; @return (integer)
  [n tags options {:keys [actual-tags left-tags] :as state}]
  (letfn [(f0 [a b] (< (:started-at a) (or (:opened-at b) (:opens-at b))))     ; <- The parent tag (as 'b') can be actually opening at the actual cursor position!
          (f1 [a b] (and (:opened-at b) (>= (:started-at a) (:opened-at b))))] ; <- The potential ascendant tag (as 'b') can be an omittag without an opening position!
         (if-let [parent-tag (parent-tag n tags options state)]
                 (loop [dex 0 left-siblings []]
                       (if ; - Iterates over the 'left-tags' vector, and counts how many left tags are direct children (not descendants) of the actual parent tag.
                           ; - When the iteration is over it returns the count of the already left children within the actual parent tag.
                           (seqable/dex-out-of-bounds? left-tags dex) (count left-siblings)
                           ; - If the observed 'left-tag' started before the parent tag opened, it means that the observed tag is not a child or even a descendant of the parent tag.
                           ; - If any other tag has been already collected into the 'left-siblings' vector during the previous iterations, it can be a potential ascendant of the observed 'left-tag'.
                           ; - If the observed 'left-tag' has ascendant(s) within the parent tag, it means that it is a descendant but not a child of the parent tag.
                           ; - If the last collected tag in the 'left-siblings' vector is an omittag, it means it cannot be an ascendant of the observed 'left-tag'.
                           ; - If a tag is currently ending at the actual cursor position, it can be a potential ascendant of any tags in the 'left-tags' vector
                           ;   and because it is not moved into the 'left-tags' vector from the 'actual-tags' vector yet, it has to be checked separatelly.
                           (let [left-tag   (nth left-tags dex)
                                 ending-tag (vector/last-match actual-tags :ends-at)]
                                (cond (f0 left-tag parent-tag)           (recur (inc dex) (->   left-siblings))
                                      (f1 left-tag ending-tag)           (recur (inc dex) (->   left-siblings))
                                      (-> left-siblings last nil?)       (recur (inc dex) (conj left-siblings left-tag))
                                      (f1 left-tag (last left-siblings)) (recur (inc dex) (->   left-siblings))
                                      :else                              (recur (inc dex) (conj left-siblings left-tag)))))))))

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
          (if-let [tag-options (tag-options n tags options state (:name parent-tag))]
                  (if (:disable-interpreter? tag-options)
                      (:name parent-tag)))))

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

(defn reading-any-opening-match?
  ; @ignore
  ;
  ; @description
  ; Returns TRUE if any opening pattern's last found match is already started but not ended yet at the actual cursor position.
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

(defn reading-any-closing-match?
  ; @ignore
  ;
  ; @description
  ; Returns TRUE if any closing pattern's last found match is already started but not ended yet at the actual cursor position.
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
  [n tags options state tag-name]
  (if-let [tag-options (tag-options n tags options state tag-name)]
          (-> tag-options :accepted-ancestors (= []))))

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
  [n tags options state tag-name]
  (if-let [tag-options (tag-options n tags options state tag-name)]
          (-> tag-options :accepted-parents (= []))))

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
  [n tags options state tag-name]
  (if-let [tag-options (tag-options n tags options state tag-name)]
          (-> tag-options :accepted-ancestors vector/nonempty?)))

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
  [n tags options state tag-name]
  (if-let [tag-options (tag-options n tags options state tag-name)]
          (-> tag-options :accepted-parents vector/nonempty?)))

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
  (if-let [tag-options (tag-options n tags options state tag-name)]
          (if-let [accepted-ancestors (:accepted-ancestors tag-options)]
                  (letfn [(f [accepted-ancestor] (tag-ancestor? n tags options state accepted-ancestor))]
                         (some f accepted-ancestors)))))

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
  (if-let [tag-options (tag-options n tags options state tag-name)]
          (if-let [accepted-parents (:accepted-parents tag-options)]
                  (letfn [(f [accepted-parent] (tag-parent? n tags options state accepted-parent))]
                         (some f accepted-parents)))))

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

(defn opening-match
  ; @ignore
  ;
  ; @description
  ; Returns the the tag name and the found match if any opening pattern's match starts at the actual cursor position.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; {:cursor (integer)}
  ; @param (keyword) tag-name
  ;
  ; @return (map)
  ; {:match (integer)
  ;  :name (keyword)}
  [n tags options {:keys [cursor] :as state} tag-name]
  ; Merging regex actions into one function decreases the interpreter processing time.
  (if-let [opening-pattern (tag-opening-pattern n tags options state tag-name)]
          (let [tag-options           (tag-options n tags options state tag-name)
                max-lookbehind-length (or (get-in tag-options                     [:pattern-limits :opening/lookbehind])
                                          (get-in tag-options                     [:pattern-limits :lookbehind])
                                          (get-in core.config/DEFAULT-TAG-OPTIONS [:pattern-limits :lookbehind]))
                max-lookahead-length  (or (get-in tag-options                     [:pattern-limits :opening/lookahead])
                                          (get-in tag-options                     [:pattern-limits :lookahead])
                                          (get-in core.config/DEFAULT-TAG-OPTIONS [:pattern-limits :lookahead]))
                max-match-length      (or (get-in tag-options                     [:pattern-limits :opening/match])
                                          (get-in tag-options                     [:pattern-limits :match])
                                          (get-in core.config/DEFAULT-TAG-OPTIONS [:pattern-limits :match]))
                corrected-cursor      (min cursor max-lookbehind-length)
                observed-from         (max (->    0) (- cursor max-lookbehind-length))
                observed-to           (min (count n) (+ cursor max-match-length max-lookahead-length))
                observed-part         (subs n observed-from observed-to)]
               (if-let [opening-match (regex/re-from observed-part opening-pattern corrected-cursor)]
                       {:name tag-name :match opening-match}))))

(defn closing-match
  ; @ignore
  ;
  ; @description
  ; Returns the the tag name and the found match if any closing pattern's match starts at the actual cursor position.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; {:cursor (integer)}
  ; @param (keyword) tag-name
  ;
  ; @return (map)
  ; {:match (integer)
  ;  :name (keyword)}
  [n tags options {:keys [cursor] :as state} tag-name]
  ; Merging regex actions into one function decreases the interpreter processing time.
  (if-let [closing-pattern (tag-closing-pattern n tags options state tag-name)]
          (let [tag-options           (tag-options n tags options state tag-name)
                max-lookbehind-length (or (get-in tag-options                     [:pattern-limits :closing/lookbehind])
                                          (get-in tag-options                     [:pattern-limits :lookbehind])
                                          (get-in core.config/DEFAULT-TAG-OPTIONS [:pattern-limits :lookbehind]))
                max-lookahead-length  (or (get-in tag-options                     [:pattern-limits :closing/lookahead])
                                          (get-in tag-options                     [:pattern-limits :lookahead])
                                          (get-in core.config/DEFAULT-TAG-OPTIONS [:pattern-limits :lookahead]))
                max-match-length      (or (get-in tag-options                     [:pattern-limits :closing/match])
                                          (get-in tag-options                     [:pattern-limits :match])
                                          (get-in core.config/DEFAULT-TAG-OPTIONS [:pattern-limits :match]))
                corrected-cursor      (min cursor max-lookbehind-length)
                observed-from         (max (->    0) (- cursor max-lookbehind-length))
                observed-to           (min (count n) (+ cursor max-match-length max-lookahead-length))
                observed-part         (subs n observed-from observed-to)]
               (if-let [closing-match (regex/re-from observed-part closing-pattern corrected-cursor)]
                       {:name tag-name :match closing-match}))))

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
  ; @param (map) opening-match
  ; {:match (string)
  ;  :name (keyword)}
  ;
  ; @example
  ; (start-child-tag "..." {...} {...}
  ;                  {:cursor 7 :actual-tags [{:name :paren :started-at 1 :opened-at 2}
  ;                                           {:name :paren :started-at 4 :opened-at 5}]}
  ;                  {:name :paren :match "("})
  ; =>
  ; {:cursor 7 :actual-tags [{:name :paren :started-at 1 :opened-at 2}
  ;                          {:name :paren :started-at 4 :opened-at 5}
  ;                          {:name :paren :starts-at  7 :will-open-at 8}]}
  ;
  ; @return (map)
  [n tags options {:keys [cursor] :as state} {:keys [match name]}]
  (letfn [(f [{:keys [closed-at opened-at opens-at]}]
             ; Tags that are already opened and aren't closed yet:
             (and (or opens-at opened-at) (not closed-at)))]
         (if (tag-omittag? n tags options state name)
             (update state :actual-tags vector/conj-item {:name name :starts-at cursor :will-end-at  (+ cursor (count match))})
             (update state :actual-tags vector/conj-item {:name name :starts-at cursor :will-open-at (+ cursor (count match))}))))

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
  ; @param (map) closing-match
  ; {:match (string)
  ;  :name (keyword)}
  ;
  ; @example
  ; (close-parent-tag "..." {...} {...}
  ;                   {:cursor 10 :actual-tags [{:name :paren :started-at 1 :opened-at 2}
  ;                                             {:name :paren :started-at 4 :opened-at 5}
  ;                                             {:name :paren :started-at 7 :opened-at 8}]}
  ;                   {:name :paren :match ")"})
  ; =>
  ; {:cursor 10 :actual-tags [{:name :paren :started-at 1 :opened-at 2}
  ;                           {:name :paren :started-at 4 :opened-at 5}
  ;                           {:name :paren :started-at 7 :opened-at 8 :closes-at 10 :will-end-at 11}]}
  ;
  ; @return (map)
  [n tags options {:keys [actual-tags cursor] :as state} {:keys [match name]}]
  (let [parent-tag     (parent-tag n tags options state)
        parent-tag-dex (vector/last-dex-of actual-tags parent-tag)]
       (update state :actual-tags vector/update-nth-item parent-tag-dex merge {:closes-at cursor :will-end-at (+ cursor (count match))})))

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

(defn archive-left-tag
  ; @ignore
  ;
  ; @description
  ; ...
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; @param (map) left-tag
  ;
  ; @return (map)
  [_ _ _ state left-tag]
  (let [left-tag (map/move left-tag :ends-at :ended-at)]
       (update state :left-tags vector/conj-item left-tag)))

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
  (letfn [(f0 [%] (cond-> % (-> % :will-open-at (=      cursor))  (map/move :will-open-at :opens-at)
                            (-> % :will-end-at  (=      cursor))  (map/move :will-end-at  :ends-at)
                            (-> % :starts-at    (= (dec cursor))) (map/move :starts-at    :started-at)
                            (-> % :opens-at     (= (dec cursor))) (map/move :opens-at     :opened-at)
                            (-> % :closes-at    (= (dec cursor))) (map/move :closes-at    :closed-at)))]
         (update state :actual-tags vector/->items f0)))

(defn actualize-updated-tags
  ; @ignore
  ;
  ; @description
  ; - Moves the currently ending tag (if any) from the 'actual-tags' tags vector into the 'left-tags' vector.
  ; - Ensures that the 'left-tags' vector is sorted by the starting positions of the left tags.
  ;   By default, it would be sorted by the ending positions if the ended tags were simply appended to the end of the vector.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ;
  ; @return (map)
  [_ _ _ {:keys [actual-tags left-tags] :as state}]
  (letfn [(f0 [a b] (> (:started-at a) (:started-at b)))]
         (if-let [ending-tag-dex (vector/last-dex-by actual-tags :ends-at)]
                 (let [ended-tag (-> actual-tags (nth ending-tag-dex) (map/move :ends-at :ended-at))]
                      (if-let [insert-dex (vector/first-dex-by left-tags #(f0 % ended-tag))]
                              (-> state (update :actual-tags vector/remove-nth-item ending-tag-dex)
                                        (update :left-tags   vector/insert-item insert-dex ended-tag))
                              (-> state (update :actual-tags vector/remove-nth-item ending-tag-dex)
                                        (update :left-tags   vector/conj-item ended-tag))))
                 (-> state))))

(defn check-for-opening-match
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
  ; {:match (integer)
  ;  :name (keyword)}
  [n tags options state]
  (letfn [(f [tag-name] (if-let [opening-match (opening-match n tags options state tag-name)]
                                (and (tag-ancestor-requirements-met? n tags options state tag-name)
                                     (tag-parent-requirements-met?   n tags options state tag-name)
                                     (-> opening-match))))]
         (and (-> (interpreter-enabled?       n tags options state))
              (-> (reading-any-opening-match? n tags options state) not)
              (-> (reading-any-closing-match? n tags options state) not)
              (or (some (fn [[tag-name _]] (f tag-name)) (map/filter-values tags (fn [[_ _ {:keys [priority] :or {priority :default}}]] (= priority :high))))
                  (some (fn [[tag-name _]] (f tag-name)) (map/filter-values tags (fn [[_ _ {:keys [priority] :or {priority :default}}]] (= priority :default))))
                  (some (fn [[tag-name _]] (f tag-name)) (map/filter-values tags (fn [[_ _ {:keys [priority] :or {priority :default}}]] (= priority :low))))))))

(defn check-for-closing-match
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
  ; {:match (integer)
  ;  :name (keyword)}
  [n tags options state]
  (and (-> (reading-any-opening-match? n tags options state) not)
       (-> (reading-any-closing-match? n tags options state) not)
       (if-let [parent-tag (parent-tag n tags options state)]
               (closing-match n tags options state (:name parent-tag)))))

(defn update-previous-state
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
  (let [state (actualize-previous-tags n tags options state)]
       (or (if-let [found-opening-match (check-for-opening-match n tags options state)]
                   (start-child-tag n tags options state found-opening-match))
           (if-let [found-closing-match (check-for-closing-match n tags options state)]
                   (close-parent-tag n tags options state found-closing-match))
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
  (cond (-> updated-result vector? not)              (-> state (assoc :result   (-> updated-result)))
        (-> updated-result first (= :$stop))         (-> state (assoc :result   (-> updated-result last) :cursor :iteration-stopped))
        (-> updated-result first (= :$set-metadata)) (-> state (assoc :metadata (-> updated-result second))
                                                               (assoc :result   (-> updated-result last)))
        :else                                        (-> state (assoc :result   (-> updated-result)))))

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
