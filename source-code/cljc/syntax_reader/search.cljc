
(ns syntax-reader.search
    (:require [map.api                   :as map]
              [string.api                :as string]
              [syntax-reader.config      :as config]
              [syntax-reader.interpreter :as interpreter]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn opening-tag-position
  ; @description
  ; - Returns the position of the first 'opening-tag' string in the given 'n' string.
  ; - If the 'offset' parameter is passed, the search starts from the offset position.
  ; - The returned position is an absolute value and it is independent from the offset value.
  ;
  ; @param (string) n
  ; @param (string) opening-tag
  ; @param (map)(opt) tags
  ; {:comment (vector)(opt)
  ;   [(string) opening-tag
  ;    (string) closing-tag
  ;    (map)(opt) tag-options
  ;     {:disable-interpreter? (boolean)(opt)}
  ;       Default: false}]
  ;  Default: [";" "\n" {:disable-interpreter? true}]
  ;  :quote (vector)(opt)
  ;   [(string) opening-tag
  ;    (string) closing-tag
  ;    (map)(opt) tag-options
  ;     {:disable-interpreter? (boolean)(opt)}
  ;       Default: false}]}
  ; @param (map)(opt) options
  ; {:endpoint (integer)(opt)
  ;   Quits searching at the endpoint position in the given 'n' string.
  ;  :ignore-commented? (boolean)(opt)
  ;   Default: true
  ;  :ignore-escaped? (boolean)(opt)
  ;   Default: true
  ;  :ignore-quoted? (boolean)(opt)
  ;   Default: true
  ;  :offset (integer)(opt)
  ;   Starts searching at the offset position in the given 'n' string.
  ;   Default: 0}
  ;
  ; @example
  ; (opening-tag-position "<div>My content</div>" "<div>")
  ; =>
  ; 0
  ;
  ; @example
  ; (opening-tag-position "<div><div></div></div>" "<div>")
  ; =>
  ; 0
  ;
  ; @example
  ; (opening-tag-position "</div> <div></div>" "<div>")
  ; =>
  ; 7
  ;
  ; @return (integer)
  ([n opening-tag]
   (opening-tag-position n opening-tag {} {}))

  ([n opening-tag tags]
   (opening-tag-position n opening-tag tags {}))

  ([n opening-tag tags {:keys [ignore-commented?      ignore-quoted?      offset]
                        :or   {ignore-commented? true ignore-quoted? true offset 0}
                        :as   options}]
   (let [; If the 'ignore-commented?' / 'ignore-quoted?' option is passed, ...
         ; ... it uses the default value of the ':comment' / ':quote' tag, if it is not provided in the given 'tags' map.
         ; ... it makes sure that the ':disable-interpreter?' option is TRUE for the ':comment' / ':quote' tag, (even if it is provided in the given 'tags' map).
         tags (cond-> tags ignore-commented? (map/reversed-merge {:comment (-> config/DEFAULT-TAGS :comment)})
                           ignore-commented? (assoc-in           [:comment 2 :disable-interpreter?] true)
                           ignore-quoted?    (map/reversed-merge {:quote   (-> config/DEFAULT-TAGS :quote)})
                           ignore-quoted?    (assoc-in           [:quote   2 :disable-interpreter?] true))]
        (letfn [; ...
                (f [_ {:keys [cursor] :as state}
                      {:keys [stop]   :as functions}]
                   (cond ; The interpreter starts the process at the 0th cursor position in order to make accurate tag map,
                         ; and this ('opening-tag-position') function starts searching for the given 'opening-tag' string from the given 'offset' position.
                         (-> offset (> cursor))
                         (-> nil)
                         ; ...
                         (string/starts-at? n opening-tag cursor)
                         (stop cursor)))]
               ; ...
               (let [initial nil]
                    ; The 'offset' parameter is handled by this ('opening-tag-position') function (not by the 'interpreter' function),
                    ; because the interpreter must start on the 0th cursor position in order to make accurate tag map for comments and quotes.
                    (interpreter/interpreter n f initial tags (dissoc options :offset)))))))

(defn closing-tag-position
  ; @description
  ; - Returns the position of the corresponding closing tag of the first occurence of the 'opening-tag' string in the given 'n' string.
  ; - If the 'offset' parameter is passed, the search starts from the offset position.
  ; - The returned position is an absolute value and it is independent from the offset value.
  ;
  ; @param (string) n
  ; @param (string) opening-tag
  ; @param (string) closing-tag
  ; @param (map)(opt) tags
  ; {:comment (vector)(opt)
  ;   [(string) opening-tag
  ;    (string) closing-tag
  ;    (map)(opt) tag-options
  ;     {:disable-interpreter? (boolean)(opt)}
  ;       Default: false}]
  ;  Default: [";" "\n" {:disable-interpreter? true}]
  ;  :quote (vector)(opt)
  ;   [(string) opening-tag
  ;    (string) closing-tag
  ;    (map)(opt) tag-options
  ;     {:disable-interpreter? (boolean)(opt)}
  ;       Default: false}]}
  ; @param (map)(opt) options
  ; {:endpoint (integer)(opt)
  ;   Quits searching at the endpoint position in the given 'n' string.
  ;  :ignore-commented? (boolean)(opt)
  ;   Default: false
  ;  :ignore-escaped? (boolean)(opt)
  ;   Default: true
  ;  :ignore-quoted? (boolean)(opt)
  ;   Default: false
  ;  :offset (integer)(opt)
  ;   Starts searching at the offset position in the given 'n' string.
  ;   Default: 0}
  ;
  ; @example
  ; (closing-tag-position "<div>My content</div>" "<div>" "</div>")
  ; =>
  ; 15
  ;
  ; @example
  ; (closing-tag-position "<div><div></div></div>" "<div>" "</div>")
  ; =>
  ; 16
  ;
  ; @example
  ; (closing-tag-position "</div> <div></div>" "<div>" "</div>")
  ; =>
  ; 12
  ;
  ; @return (integer)
  ([n opening-tag closing-tag]
   (closing-tag-position n opening-tag closing-tag {} {}))

  ([n opening-tag closing-tag tags]
   (closing-tag-position n opening-tag closing-tag tags {}))

  ([n opening-tag closing-tag tags {:keys [ignore-commented?      ignore-quoted?      offset]
                                    :or   {ignore-commented? true ignore-quoted? true offset 0}
                                    :as   options}]
   (let [; If the 'ignore-commented?' / 'ignore-quoted?' option is passed, ...
         ; ... it uses the default value of the ':comment' / ':quote' tag, if it is not provided in the given 'tags' map.
         ; ... it makes sure that the ':disable-interpreter?' option is TRUE for the ':comment' / ':quote' tag, (even if it is provided in the given 'tags' map).
         tags (cond-> tags ignore-commented? (map/reversed-merge {:comment (-> config/DEFAULT-TAGS :comment)})
                           ignore-commented? (assoc-in           [:comment 2 :disable-interpreter?] true)
                           ignore-quoted?    (map/reversed-merge {:quote   (-> config/DEFAULT-TAGS :quote)})
                           ignore-quoted?    (assoc-in           [:quote   2 :disable-interpreter?] true))]
        (letfn [; ...
                (f [_ {:keys [cursor]                                    :as state}
                      {:keys [closing-tag-starts? stop tag-actual-depth] :as functions}]
                   (cond ; The interpreter starts the process at the 0th cursor position in order to make accurate tag map,
                         ; and this ('closing-tag-position') function starts searching for the given 'closing-tag' string from the given 'offset' position.
                         (-> offset (> cursor))
                         (-> nil)
                         ; If the given tag is closing (the given 'closing-tag' value starts at the actual cursor position),
                         ; and it closes its 1st depth (inner pairs of the given 'opening-tag' and 'closing-tag' strings have higher depth value than 1) ...
                         (and (-> ::tag closing-tag-starts?)
                              (-> ::tag tag-actual-depth (= 1)))
                         ; ... returns the actual cursor position.
                         (stop cursor)))]
               ; ...
               (let [initial nil]
                    ; The 'offset' parameter is handled by this ('closing-tag-position') function (not by the 'interpreter' function),
                    ; because the interpreter must start on the 0th cursor position in order to make accurate tag map for comments and quotes.
                    (interpreter/interpreter n f initial (assoc  tags ::tag [opening-tag closing-tag])
                                                         (dissoc options :offset)))))))
