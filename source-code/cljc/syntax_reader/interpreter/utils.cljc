
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
  ; {:actual-tags (vectors in vector)}
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
  (seqable/last-cursor? n cursor))

(defn endpoint-reached?
  ; @ignore
  ;
  ; @description
  ; Returns TRUE if the actual cursor position exceeded the given endpoint value.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; {:endpoint (integer)(opt)}
  ; @param (map) state
  ; {:cursor (integer)}
  ;
  ; @return (boolean)
  [n _ {:keys [endpoint]} {:keys [cursor]}]
  (and endpoint (= cursor endpoint)))

(defn interpreter-stopped?
  ; @ignore
  ;
  ; @description
  ; Returns TRUE if the 'stop' metafunction stopped the iteration by ...
  ; ... wrapping the 'result' value within a vector,
  ; ... putting the '::$stop' marker into the wrapping vector as its first item.
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
       (-> result first (= ::$stop))))

;; -- State functions ---------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn filter-provided-state
  ; @ignore
  ;
  ; @description
  ; Filters the given state for the keys that are presented in the provided state (provided for the applied 'f' function).
  ;
  ; @param (map) state
  ;
  ; @return (map)
  [state]
  (select-keys state [:actual-tags :cursor]))

;; -- Tag functions -----------------------------------------------------------
;; ----------------------------------------------------------------------------

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
  ; {:actual-tags (vectors in vector)}
  ; @param (keyword) tag-name
  ;
  ; @return (integer)
  [_ _ _ {:keys [actual-tags]} tag-name]
  (letfn [(f [actual-tag] (and (-> actual-tag :name (= tag-name))
                               (-> actual-tag :opened-at integer?)))]
         (vector/match-count actual-tags f)))

(defn tag-opened?
  ; @ignore
  ;
  ; @description
  ; Returns whether the given tag is opened.
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

(defn tag-opened-at
  ; @ignore
  ;
  ; @description
  ; Returns the opening cursor position of the given tag.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; {:actual-tags (vectors in vector)}
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
  ; Returns the starting cursor position of the given tag.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; {:actual-tags (vectors in vector)}
  ; @param (keyword) tag-name
  ;
  ; @return (integer)
  [_ _ _ {:keys [actual-tags]} tag-name]
  (letfn [(f [actual-tag] (-> actual-tag :name (= tag-name)))]
         (if-let [innermost-tag (vector/last-match actual-tags f)]
                 (:started-at innermost-tag))))

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
  ; Returns the cursor position where the given tag's opening tag will end.
  ; (actual cursor position + tag's opening tag length)
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

(defn tag-not-requires-accepted-parent?
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

(defn tag-accepted-parent-opened?
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

(defn open-tag-innermost-depth
  ; @ignore
  ;
  ; @description
  ; Updates the given 'state' by opening a new innermost depth for the given tag in the 'actual-tags' vector.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; {:cursor (integer)}
  ; @param (keyword) tag-name
  ;
  ; @example
  ; (open-tag-innermost-depth "..." {...} {...}
  ;                           {:cursor 7 :actual-tags [{:name :paren :depth 1 :started-at 1 :opened-at 2}
  ;                                                    {:name :paren :depth 2 :started-at 4 :opened-at 5}]}
  ;                           :paren)
  ; =>
  ; {:cursor 7 :actual-tags [{:name :paren :depth 1 :started-at 1 :opened-at 2}
  ;                          {:name :paren :depth 2 :started-at 4 :opened-at 5}
  ;                          {:name :paren :depth 3 :started-at 7 :opened-at 8}]}
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

(defn close-tag-innermost-depth
  ; @ignore
  ;
  ; @description
  ; - Updates the given 'state' by closing the innermost depth of the given tag in the 'actual-tags' vector.
  ; - Removes the closed tag's descendant tags also (in case of somehow they aren't closed / removed yet).
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; @param (keyword) tag-name
  ;
  ; @example
  ; (close-tag-innermost-depth "..." {...} {...}
  ;                            {:cursor 10 :actual-tags [{:name :paren :depth 1 :started-at 1 :opened-at 2}
  ;                                                      {:name :paren :depth 2 :started-at 4 :opened-at 5}
  ;                                                      {:name :paren :depth 3 :started-at 7 :opened-at 8}]}
  ;                            :paren)
  ; =>
  ; {:cursor 10 :actual-tags [{:name :paren :depth 1 :started-at 1 :opened-at 2}
  ;                           {:name :paren :depth 2 :started-at 4 :opened-at 5}]}
  ;
  ; @return (map)
  [_ _ _ state tag-name]
  (letfn [(f [actual-tag] (-> actual-tag :name (= tag-name)))]
         (update state :actual-tags vector/before-last-match f)))

;; -- Actual state functions --------------------------------------------------
;; ----------------------------------------------------------------------------

(defn innermost-tag-unfinished?
  [_ _ _ {:keys [actual-tags cursor]}]
  (or (-> actual-tags last :will-open-at (> cursor))
      (-> actual-tags last :will-end-at  (> cursor))))

(defn actualize-innermost-tag
  [_ _ _ {:keys [actual-tags cursor] :as state}]
  (cond (-> actual-tags last :will-open-at (= cursor)) (-> state (update :actual-tags vector/update-last-item map/rekey-item :will-open-at :opened-at))
        (-> actual-tags last :will-end-at  (= cursor)) (-> state (update :actual-tags vector/remove-last-item))))

(defn actualize-tag-opening-positions
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
  (letfn [(f [{:keys [will-open-at] :as actual-tag}]
             (if (-> will-open-at (= cursor))
                 (-> actual-tag (map/rekey-item :will-open-at :opened-at))
                 (-> actual-tag)))]
         (update state :actual-tags vector/->items f)))

(defn remove-ended-tags
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
  [_ _ _ {:keys [cursor] :as state}])






(defn add-started-tag
  ; @ignore
  ;
  ; @description
  ; ...
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
                             (or (tag-not-requires-accepted-parent? n tags options state tag-name)
                                 (tag-accepted-parent-opened?       n tags options state tag-name))
                             (-> tag-name)))]
         (if-let [found-opening-tag (or (some (fn [tag-name]     (f tag-name)) tag-priority-order)
                                        (some (fn [[tag-name _]] (f tag-name)) tags))]
                 (open-tag-innermost-depth n tags options state found-opening-tag)
                 (-> state))))

(defn remove-closed-tag
  ; @ignore
  ;
  ; @description
  ; ...
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; {:actual-tags (vector)}
  ;
  ; @return (map)
  [n tags options {:keys [actual-tags] :as state}]
  (letfn [(f [{:keys [name]}] (closing-tag-starts? n tags options state name))]
         (if-let [found-closing-tag (:name (vector/last-match actual-tags f))]
                 (close-tag-innermost-depth n tags options state found-closing-tag)
                 (-> state))))

(defn remove-closed-disabling-tag
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
  (let [interpreter-disabled-by (interpreter-disabled-by n tags options state)]
       (if (closing-tag-started?      n tags options state interpreter-disabled-by)
           (close-tag-innermost-depth n tags options state interpreter-disabled-by)
           (-> state))))

(defn update-actual-state
  ; @ignore
  ;
  ; @description
  ; - Actualizes the opening positions of each tag in the 'actual-tags' vector that
  ;   are started but not opened yet and opens at the actual cursor position.
  ; - If no opened tag disables the interpreter (after the actualization) ...
  ;   ... it searches for tags that start at the actual cursor position.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ;
  ; @return (map)
  [n tags options state]
  (let [state (->> state (actualize-innermost-tag n tags options)    ; <- nem kell a többes szám ha egyszerre csak egy tag nyitasa lehet folyamatban
                         (remove-ended-tags               n tags options))]  ; <- ez is singular cucc legyen
       (if-not (interpreter-disabled? n tags options state)
               (add-started-tag       n tags options state)
               (-> state))))

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
  (if-not (interpreter-disabled? n tags options state)))

  ; a zárás is legyen egy folyamat mint a nyitás
  ; :will-end-at
  ; zárás / nyitás közben ne legyen feldolgozás
