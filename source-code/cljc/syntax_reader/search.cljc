
(ns syntax-reader.search
    (:require [map.api                   :as map]
              [regex.api                 :as regex]
              [syntax-reader.core.config      :as core.config]
              [syntax-reader.interpreter.core :as interpreter.core]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn first-position
  ; @description
  ; - Returns the position of the first 'x' regex pattern in the given 'n' string.
  ; - If the 'offset' parameter is passed, the search starts from the offset position.
  ; - The returned position is an absolute value and it is independent from the offset value.
  ;
  ; @param (string) n
  ; @param (regex pattern) x
  ; @param (map)(opt) tags
  ; {:comment (vector)(opt)
  ;   [(regex pattern) opening-tag
  ;    (regex pattern) closing-tag
  ;    (map)(opt) tag-options
  ;     {:disable-interpreter? (boolean)(opt)}
  ;       Default: false}]
  ;  Default: [#";" #"\n" {:disable-interpreter? true}]
  ;  :quote (vector)(opt)
  ;   [(regex pattern) opening-tag
  ;    (regex pattern) closing-tag
  ;    (map)(opt) tag-options
  ;     {:disable-interpreter? (boolean)(opt)}
  ;       Default: false}]}
  ;  Default: [#"\"" #"\"" {:disable-interpreter? true}]
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
  ; (first-position ".my-class { width: auto}" #"width")
  ; =>
  ; 12
  ;
  ; @example
  ; (first-position ".my-class {/* width: 0 */ width: auto}" #"width" {:comment [#"/\*" #"\*/" {:disable-interpreter? true}]})
  ; =>
  ; 26
  ;
  ; @return (integer)
  ([n x]
   (first-position n x {} {}))

  ([n x tags]
   (first-position n x tags {}))

  ([n x tags {:keys [ignore-commented?      ignore-quoted?      offset]
              :or   {ignore-commented? true ignore-quoted? true offset 0}
              :as   options}]
   (letfn [; @description
           ; If the 'ignore-commented?' / 'ignore-quoted?' option is passed, ...
           ; ... it uses the default value of the ':comment' / ':quote' tag, if the tag is not provided in the given 'tags' map.
           ; ... it makes sure that the ':disable-interpreter?' option is TRUE for the ':comment' / ':quote' tag, (even if the tag is provided in the given 'tags' map).
           ;
           ; @return (map)
           (f0 []
               (cond-> tags ignore-commented? (-> (map/reversed-merge {:comment (-> core.config/DEFAULT-TAGS :comment)})
                                                  (assoc-in           [:comment 2 :disable-interpreter?] true))
                            ignore-quoted?    (-> (map/reversed-merge {:quote   (-> core.config/DEFAULT-TAGS :quote)})
                                                  (assoc-in           [:quote   2 :disable-interpreter?] true))))

           ; ...
           (f1 [_ {:keys [cursor]                     :as state}
                  {:keys [interpreter-disabled? stop] :as metafunctions}]
               (println state)
               (cond ; The interpreter starts the process at the 0th cursor position in order to make accurate tag map,
                     ; and this ('first-position') function starts searching for the given 'x' regex pattern from the given 'offset' position.
                     (-> offset (> cursor))
                     (-> nil)
                     ; If a ':comment' / ':quote' tag disables the interpreter and the 'ignore-commented?' / 'ignore-quoted?' option is passed.
                     (interpreter-disabled?)
                     (-> nil)
                     ; ...
                     (regex/starts-at? n x cursor)
                     (stop cursor)))]
          ; ...
          (let [initial {}
                options (dissoc options :offset)
                tags    (f0)]
               (interpreter.core/interpreter n f1 initial tags options)))))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn opening-tag-position
  ; @description
  ; - Returns the position of the first 'opening-tag' regex pattern in the given 'n' string.
  ; - If the 'offset' parameter is passed, the search starts from the offset position.
  ; - The returned position is an absolute value and it is independent from the offset value.
  ;
  ; @param (string) n
  ; @param (regex pattern) opening-tag
  ; @param (map)(opt) tags
  ; {:comment (vector)(opt)
  ;   [(regex pattern) opening-tag
  ;    (regex pattern) closing-tag
  ;    (map)(opt) tag-options
  ;     {:disable-interpreter? (boolean)(opt)}
  ;       Default: false}]
  ;  Default: [#";" #"\n" {:disable-interpreter? true}]
  ;  :quote (vector)(opt)
  ;   [(regex pattern) opening-tag
  ;    (regex pattern) closing-tag
  ;    (map)(opt) tag-options
  ;     {:disable-interpreter? (boolean)(opt)}
  ;       Default: false}]}
  ;  Default: [#"\"" #"\"" {:disable-interpreter? true}]
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
  ; (opening-tag-position "<div>My content</div>" #"<div>")
  ; =>
  ; 0
  ;
  ; @example
  ; (opening-tag-position "<div><div></div></div>" #"<div>")
  ; =>
  ; 0
  ;
  ; @example
  ; (opening-tag-position "</div> <div></div>" #"<div>")
  ; =>
  ; 7
  ;
  ; @return (integer)
  ([n opening-tag]
   (opening-tag-position n opening-tag {} {}))

  ([n opening-tag tags]
   (opening-tag-position n opening-tag tags {}))

  ([n opening-tag tags options]
   (first-position n opening-tag tags options)))

(defn closing-tag-position
  ; @description
  ; - Returns the position of the corresponding closing tag of the first occurence of the 'opening-tag' regex pattern in the given 'n' string.
  ; - If the 'offset' parameter is passed, the search starts from the offset position.
  ; - The returned position is an absolute value and it is independent from the offset value.
  ;
  ; @param (string) n
  ; @param (regex pattern) opening-tag
  ; @param (regex pattern) closing-tag
  ; @param (map)(opt) tags
  ; {:comment (vector)(opt)
  ;   [(regex pattern) opening-tag
  ;    (regex pattern) closing-tag
  ;    (map)(opt) tag-options
  ;     {:disable-interpreter? (boolean)(opt)}
  ;       Default: false}]
  ;  Default: [#";" #"\n" {:disable-interpreter? true}]
  ;  :quote (vector)(opt)
  ;   [(regex pattern) opening-tag
  ;    (regex pattern) closing-tag
  ;    (map)(opt) tag-options
  ;     {:disable-interpreter? (boolean)(opt)}
  ;       Default: false}]}
  ;  Default: [#"\"" #"\"" {:disable-interpreter? true}]
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
  ; (closing-tag-position "<div>My content</div>" #"<div>" #"</div>")
  ; =>
  ; 15
  ;
  ; @example
  ; (closing-tag-position "<div><div></div></div>" #"<div>" #"</div>")
  ; =>
  ; 16
  ;
  ; @example
  ; (closing-tag-position "</div> <div></div>" #"<div>" #"</div>")
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
   (letfn [; @description
           ; If the 'ignore-commented?' / 'ignore-quoted?' option is passed, ...
           ; ... it uses the default value of the ':comment' / ':quote' tag, if the tag is not provided in the given 'tags' map.
           ; ... it makes sure that the ':disable-interpreter?' option is TRUE for the ':comment' / ':quote' tag, (even if the tag is provided in the given 'tags' map).
           ;
           ; @return (map)
           (f0 []
               (cond-> tags ignore-commented? (-> (map/reversed-merge {:comment (-> core.config/DEFAULT-TAGS :comment)})
                                                  (assoc-in           [:comment 2 :disable-interpreter?] true))
                            ignore-quoted?    (-> (map/reversed-merge {:quote   (-> core.config/DEFAULT-TAGS :quote)})
                                                  (assoc-in           [:quote   2 :disable-interpreter?] true))
                            :add-searched-tag (-> (assoc              ::tag [opening-tag closing-tag]))))

           ; ...
           (f1 [_ {:keys [cursor]                                                          :as state}
                  {:keys [closing-tag-starts? interpreter-disabled? stop tag-actual-depth] :as metafunctions}]
               (cond ; The interpreter starts the process at the 0th cursor position in order to make accurate tag map,
                     ; and this ('closing-tag-position') function starts searching for the given 'closing-tag' regex pattern from the given 'offset' position.
                     (-> offset (> cursor))
                     (-> nil)
                     ; If a ':comment' / ':quote' tag disables the interpreter and the 'ignore-commented?' / 'ignore-quoted?' option is passed.
                     (interpreter-disabled?)
                     (-> nil)
                     ; If the given tag is closing (the given 'closing-tag' regex pattern starts at the actual cursor position),
                     ; and it also closes its 1st depth (inner pairs of the given 'opening-tag' and 'closing-tag' regex patterns have higher depth value than 1) ...
                     (and (-> ::tag closing-tag-starts?)
                          (-> ::tag tag-actual-depth (= 1)))
                     ; ... returns the actual cursor position.
                     (stop cursor)))]
          ; ...
          (let [initial {}
                options (dissoc options :offset)
                tags    (f0)]
               (interpreter.core/interpreter n f1 initial tags options)))))
