
(ns syntax-reader.interpreter.metafunctions
    (:require [syntax-reader.interpreter.utils :as interpreter.utils]
              [string.api                      :as string]
              [vector.api                      :as vector]))

;; -- Ancestor / parent tag metafunctions -------------------------------------
;; ----------------------------------------------------------------------------

(defn no-tags-opened-f
  ; @ignore
  ;
  ; @description
  ; Returns the 'no-tags-opened?' metafunction.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ;
  ; @return (boolean)
  [n tags options state]
  ; @description
  ; Returns TRUE if there is no opened tag at the actual cursor position.
  ;
  ; @return (boolean)
  (fn [] (interpreter.utils/no-tags-opened? n tags options state)))

(defn depth-f
  ; @ignore
  ;
  ; @description
  ; Returns the 'depth' metafunction.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ;
  ; @return (function)
  [n tags options state]
  ; @description
  ; Returns the depth of the actual cursor position.
  ;
  ; @return (integer)
  (fn [] (interpreter.utils/depth n tags options state)))

(defn tag-depth-f
  ; @ignore
  ;
  ; @description
  ; Returns the 'tag-depth' metafunction.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ;
  ; @return (function)
  [n tags options state]
  ; @description
  ; Returns the actual opened depth of a specific tag.
  ;
  ; @param (keyword) tag-name
  ;
  ; @return (integer)
  (fn [tag-name] (interpreter.utils/tag-depth n tags options state tag-name)))

(defn ancestor-tags-f
  ; @ignore
  ;
  ; @description
  ; Returns the 'ancestor-tags' metafunction.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ;
  ; @return (function)
  [n tags options state]
  ; @description
  ; Returns the ancestor tags of the actual cursor position.
  ;
  ; @param (keyword) tag-name
  ;
  ; @return (maps in vector)
  (fn [] (interpreter.utils/ancestor-tags n tags options state)))

(defn parent-tag-f
  ; @ignore
  ;
  ; @description
  ; Returns the 'parent-tag' metafunction.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ;
  ; @return (function)
  [n tags options state]
  ; @description
  ; Returns the parent tag of the actual cursor position.
  ;
  ; @return (map)
  (fn [] (interpreter.utils/parent-tag n tags options state)))

(defn tag-ancestor-f
  ; @ignore
  ;
  ; @description
  ; Returns the 'tag-ancestor?' metafunction.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ;
  ; @return (function)
  [n tags options state]
  ; @description
  ; Returns TRUE the given tag is an opened ancestor tag of the actual cursor position.
  ;
  ; @param (keyword) tag-name
  ;
  ; @return (boolean)
  (fn [tag-name] (interpreter.utils/tag-ancestor? n tags options state tag-name)))

(defn tag-parent-f
  ; @ignore
  ;
  ; @description
  ; Returns the 'tag-parent?' metafunction.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ;
  ; @return (function)
  [n tags options state]
  ; @description
  ; Returns TRUE if the given tag is the opened parent tag of the actual cursor position.
  ;
  ; @param (keyword) tag-name
  ;
  ; @return (boolean)
  (fn [tag-name] (interpreter.utils/tag-parent? n tags options state tag-name)))

(defn left-sibling-count-f
  ; @ignore
  ;
  ; @description
  ; Returns the 'left-sibling-count' metafunction.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ;
  ; @return (function)
  [n tags options state]
  ; @description
  ; Returns how many siblings have been already left behind by the interpreter within the actual parent tag.
  ;
  ; @return (integer)
  (fn [] (interpreter.utils/left-sibling-count n tags options state)))

;; -- Interpreter metafunctions -----------------------------------------------
;; ----------------------------------------------------------------------------

(defn interpreter-disabled-by-f
  ; @ignore
  ;
  ; @description
  ; Returns the 'interpreter-disabled-by' metafunction.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ;
  ; @return (function)
  [n tags options state]
  ; @description
  ; Returns the disabling tag's name if the interpreter is disabled by an opened tag.
  ;
  ; @return (keyword)
  (fn [] (interpreter.utils/interpreter-disabled-by n tags options state)))

(defn interpreter-disabled-f
  ; @ignore
  ;
  ; @description
  ; Returns the 'interpreter-disabled?' metafunction.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ;
  ; @return (function)
  [n tags options state]
  ; @description
  ; Returns TRUE if the interpreter is disabled by an opened tag.
  ;
  ; @return (boolean)
  (fn [] (interpreter.utils/interpreter-disabled? n tags options state)))

(defn interpreter-enabled-f
  ; @ignore
  ;
  ; @description
  ; Returns the 'interpreter-enabled?' metafunction.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ;
  ; @return (function)
  [n tags options state]
  ; @description
  ; Returns TRUE if the interpreter is NOT disabled by an opened tag.
  ;
  ; @return (boolean)
  (fn [] (interpreter.utils/interpreter-enabled? n tags options state)))

(defn reading-any-opening-match-f
  ; @ignore
  ;
  ; @description
  ; Returns the 'reading-any-opening-match?' metafunction.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ;
  ; @return (function)
  [n tags options state]
  ; @description
  ; Returns TRUE if any opening pattern's last found match is already started but not ended yet at the actual cursor position.
  ;
  ; @return (boolean)
  (fn [] (interpreter.utils/reading-any-opening-match? n tags options state)))

(defn reading-any-closing-match-f
  ; @ignore
  ;
  ; @description
  ; Returns the 'reading-any-closing-match?' metafunction.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ;
  ; @return (function)
  [n tags options state]
  ; @description
  ; Returns TRUE if any closing pattern's last found match is already started but not ended yet at the actual cursor position.
  ;
  ; @return (boolean)
  (fn [] (interpreter.utils/reading-any-closing-match? n tags options state)))

;; -- Operator metafunctions --------------------------------------------------
;; ----------------------------------------------------------------------------

(defn stop-f
  ; @ignore
  ;
  ; @description
  ; Returns the 'stop' metafunction.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ;
  ; @return (function)
  [_ _ _ _]
  ; @description
  ; Stops the interpreter immediatelly and the interpreter returns the parameter of this ('stop') function.
  ;
  ; @param (*) result
  ;
  ; @usage
  ; (stop "My output")
  ;
  ; @return (vector)
  ; [(keyword) stop-marker
  ;  (*) result]
  (fn [result] [:$stop result]))

(defn set-state-f
  ; @ignore
  ;
  ; @description
  ; Returns the 'set-state' metafunction.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ;
  ; @return (function)
  [_ _ _ _]
  ; @description
  ; Merges the given 'x' map onto the 'state' map before the next iteration.
  ;
  ; @param (map) x
  ; {:result (*)(opt)}
  ;
  ; @usage
  ; (set-state {:my-value "This will be available in the state from the next iteration."
  ;             :result "My output"})
  ;
  ; @return (vector)
  ; [(keyword) set-state-marker
  ;  (map) extra]
  (fn [x] [:$set-state x]))

;; -- Tag boundary metafunctions ----------------------------------------------
;; ----------------------------------------------------------------------------

(defn starting-tag-f
  ; @ignore
  ;
  ; @description
  ; Returns the 'starting-tag' metafunction.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; {:actual-tags (maps in vector)
  ;  :cursor (integer)}
  ;
  ; @return (function)
  [_ _ _ {:keys [actual-tags cursor]}]
  ; @description
  ; Returns the tag's name that starts at the actual cursor position (if any).
  ;
  ; @return (keyword)
  (fn [] (letfn [(f [%] (-> % :starts-at (= cursor)))]
                (-> (vector/last-match actual-tags f) :name))))

(defn opening-tag-f
  ; @ignore
  ;
  ; @description
  ; Returns the 'opening-tag' metafunction.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; {:actual-tags (maps in vector)
  ;  :cursor (integer)}
  ;
  ; @return (function)
  [_ _ _ {:keys [actual-tags cursor]}]
  ; @description
  ; Returns the tag's name that opens at the actual cursor position (if any).
  ;
  ; @return (keyword)
  (fn [] (letfn [(f [%] (-> % :opens-at (= cursor)))]
                (-> (vector/last-match actual-tags f) :name))))

(defn closing-tag-f
  ; @ignore
  ;
  ; @description
  ; Returns the 'closing-tag' metafunction.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; {:actual-tags (maps in vector)
  ;  :cursor (integer)}
  ;
  ; @return (function)
  [_ _ _ {:keys [actual-tags cursor]}]
  ; @description
  ; Returns the tag's name that closes at the actual cursor position (if any).
  ;
  ; @return (keyword)
  (fn [] (letfn [(f [%] (-> % :closes-at (= cursor)))]
                (-> (vector/last-match actual-tags f) :name))))

(defn ending-tag-f
  ; @ignore
  ;
  ; @description
  ; Returns the 'ending-tag' metafunction.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; {:actual-tags (maps in vector)
  ;  :cursor (integer)}
  ;
  ; @return (function)
  [_ _ _ {:keys [actual-tags cursor]}]
  ; @description
  ; Returns the tag's name that ends at the actual cursor position (if any).
  ;
  ; @return (keyword)
  (fn [] (letfn [(f [%] (-> % :ends-at (= cursor)))]
                (-> (vector/last-match actual-tags f) :name))))

(defn tag-starts-f
  ; @ignore
  ;
  ; @description
  ; Returns the 'tag-starts?' metafunction.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; {:actual-tags (maps in vector)
  ;  :cursor (integer)}
  ;
  ; @return (function)
  [_ _ _ {:keys [actual-tags cursor]}]
  ; @description
  ; Returns TRUE if the given tag's opening pattern's match starts at the actual cursor position.
  ;
  ; @param (keyword) tag-name
  ;
  ; @return (boolean)
  (fn [tag-name] (letfn [(f [%] (and (-> % :name      (= tag-name))
                                     (-> % :starts-at (= cursor))))]
                        (vector/any-item-matches? actual-tags f))))

(defn tag-started-f
  ; @ignore
  ;
  ; @description
  ; Returns the 'tag-started?' metafunction.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; {:actual-tags (maps in vector)
  ;  :cursor (integer)}
  ;
  ; @return (function)
  [_ _ _ {:keys [actual-tags cursor]}]
  ; @description
  ; Returns TRUE if the given tag is started.
  ;
  ; @param (keyword) tag-name
  ;
  ; @return (boolean)
  (fn [tag-name] (letfn [(f [%] (and (-> % :name (= tag-name))
                                     (or (-> % :starts-at (= cursor))
                                         (-> % :started-at integer?))))]
                        (vector/any-item-matches? actual-tags f))))

(defn tag-started-at-f
  ; @ignore
  ;
  ; @description
  ; Returns the 'tag-started-at' metafunction.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; {:actual-tags (maps in vector)}
  ;
  ; @return (function)
  [_ _ _ {:keys [actual-tags]}]
  ; @description
  ; Returns the starting position of the given tag.
  ;
  ; @param (keyword) tag-name
  ;
  ; @return (integer)
  (fn [tag-name] (letfn [(f [%] (and (-> % :name (= tag-name))
                                     (or (-> % :starts-at)
                                         (-> % :started-at))))]
                        (some f actual-tags))))

(defn tag-opens-f
  ; @ignore
  ;
  ; @description
  ; Returns the 'tag-opens?' metafunction.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; {:actual-tags (maps in vector)
  ;  :cursor (integer)}
  ;
  ; @return (function)
  [_ _ _ {:keys [actual-tags cursor]}]
  ; @description
  ; Returns TRUE if the given tag's opening pattern's match ends at the actual cursor position.
  ;
  ; @param (keyword) tag-name
  ;
  ; @return (boolean)
  (fn [tag-name] (letfn [(f [%] (and (-> % :name     (= tag-name))
                                     (-> % :opens-at (= cursor))))]
                        (vector/any-item-matches? actual-tags f))))

(defn tag-opened-f
  ; @ignore
  ;
  ; @description
  ; Returns the 'tag-opened?' metafunction.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; {:actual-tags (maps in vector)
  ;  :cursor (integer)}
  ;
  ; @return (function)
  [_ _ _ {:keys [actual-tags cursor]}]
  ; @description
  ; Returns TRUE if the given tag is opened.
  ;
  ; @param (keyword) tag-name
  ;
  ; @return (boolean)
  (fn [tag-name] (letfn [(f [%] (and (-> % :name (= tag-name))
                                     (or (-> % :opens-at (= cursor))
                                         (-> % :opened-at integer?))
                                     (-> % :closed-at not)))]
                        (vector/any-item-matches? actual-tags f))))

(defn tag-opened-at-f
  ; @ignore
  ;
  ; @description
  ; Returns the 'tag-opened-at' metafunction.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; {:actual-tags (maps in vector)}
  ;
  ; @return (function)
  [_ _ _ {:keys [actual-tags]}]
  ; @description
  ; Returns the opening position of the given tag.
  ;
  ; @param (keyword) tag-name
  ;
  ; @return (integer)
  (fn [tag-name] (letfn [(f [%] (and (-> % :name (= tag-name))
                                     (or (-> % :opens-at)
                                         (-> % :opened-at))))]
                        (some f actual-tags))))

(defn tag-closes-f
  ; @ignore
  ;
  ; @description
  ; Returns the 'tag-closes?' metafunction.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; {:actual-tags (maps in vector)
  ;  :cursor (integer)}
  ;
  ; @return (function)
  [_ _ _ {:keys [actual-tags cursor]}]
  ; @description
  ; Returns TRUE if the given tag's closing pattern's match starts at the actual cursor position.
  ;
  ; @param (keyword) tag-name
  ;
  ; @return (boolean)
  (fn [tag-name] (letfn [(f [%] (and (-> % :name      (= tag-name))
                                     (-> % :closes-at (= cursor))))]
                        (vector/any-item-matches? actual-tags f))))

(defn tag-closed-f
  ; @ignore
  ;
  ; @description
  ; Returns the 'tag-closed?' metafunction.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; {:actual-tags (maps in vector)
  ;  :cursor (integer)}
  ;
  ; @return (function)
  [_ _ _ {:keys [actual-tags cursor]}]
  ; @description
  ; Returns TRUE if the given tag is closed.
  ;
  ; @param (keyword) tag-name
  ;
  ; @return (boolean)
  (fn [tag-name] (letfn [(f [%] (and (-> % :name (= tag-name))
                                     (or (-> % :closes-at (= cursor))
                                         (-> % :closed-at integer?))))]
                        (vector/any-item-matches? actual-tags f))))

(defn tag-closed-at-f
  ; @ignore
  ;
  ; @description
  ; Returns the 'tag-closed-at' metafunction.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; {:actual-tags (maps in vector)}
  ;
  ; @return (function)
  [_ _ _ {:keys [actual-tags]}]
  ; @description
  ; Returns the closing position of the given tag.
  ;
  ; @param (keyword) tag-name
  ;
  ; @return (integer)
  (fn [tag-name] (letfn [(f [%] (and (-> % :name (= tag-name))
                                     (or (-> % :closes-at)
                                         (-> % :closed-at))))]
                        (some f actual-tags))))

(defn tag-ends-f
  ; @ignore
  ;
  ; @description
  ; Returns the 'tag-ends?' metafunction.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; {:actual-tags (maps in vector)
  ;  :cursor (integer)}
  ;
  ; @return (function)
  [_ _ _ {:keys [actual-tags cursor]}]
  ; @description
  ; Returns TRUE if the given tag's closing pattern's match ends at the actual cursor position.
  ;
  ; @param (keyword) tag-name
  ;
  ; @return (boolean)
  (fn [tag-name] (letfn [(f [%] (and (-> % :name    (= tag-name))
                                     (-> % :ends-at (= cursor))))]
                        (vector/any-item-matches? actual-tags f))))

;; -- Tag body / content metafunctions ----------------------------------------
;; ----------------------------------------------------------------------------

(defn tag-body-f
  ; @ignore
  ;
  ; @description
  ; Returns the 'tag-body' metafunction.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; {:cursor (integer)}
  ;
  ; @return (function)
  [n tags options {:keys [cursor] :as state}]
  (fn [tag-name] (if ((tag-started-f n tags options state) tag-name)
                     (let [tag-started-at ((tag-started-at-f n tags options state) tag-name)]
                          (string/keep-range n tag-started-at cursor)))))

(defn tag-content-f
  ; @ignore
  ;
  ; @description
  ; Returns the 'tag-content' metafunction.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ; {:cursor (integer)}
  ;
  ; @return (function)
  [n tags options {:keys [cursor] :as state}]
  (fn [tag-name] (if ((tag-opened-f n tags options state) tag-name)
                     (let [tag-opened-at ((tag-opened-at-f n tags options state) tag-name)]
                          (string/keep-range n tag-opened-at cursor)))))
