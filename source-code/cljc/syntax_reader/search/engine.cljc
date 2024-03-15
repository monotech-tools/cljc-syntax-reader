
(ns syntax-reader.search.engine
    (:require [fruits.vector.api             :as vector]
              [syntax-interpreter.api        :as syntax-interpreter]
              [syntax-reader.core.prototypes :as core.prototypes]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn tag-starting-position
  ; @links
  ; [cljc-syntax-interpreter](https://mt-devtools.github.io/cljc-syntax-interpreter)
  ;
  ; @description
  ; - Returns the starting position of the given searched tag in the given 'n' string.
  ; - By default, it ignores commented and quoted parts of the string.
  ;
  ; @param (string) n
  ; @param (vector) searched-tag
  ; [(keyword) tag-name
  ;  (regex pattern) pattern / opening-pattern
  ;  (regex pattern)(opt) closing-pattern
  ;  (map)(opt) options]
  ; @param (vectors in vector)(opt)(in decreasing priority order) tags
  ; [[(keyword) tag-name
  ;   (regex pattern) pattern / opening-pattern
  ;   (regex pattern)(opt) closing-pattern
  ;   (map)(opt) tag-options]]
  ; Default:
  ; [[:comment       #"\;"   #"\n" {:accepted-children []}]
  ;  [:meta-string   #"\^\"" #"\"" {:accepted-children []}]
  ;  [:regex-pattern #"\#\"" #"\"" {:accepted-children []}]
  ;  [:string        #"\""   #"\"" {:accepted-children []}]]
  ; @param (map)(opt) options
  ; {:endpoint (integer)(opt)
  ;   Quits searching at the given 'endpoint' position in the given 'n' string.
  ;  :offset (integer)(opt)
  ;   Starts searching at the given 'offset' position in the given 'n' string.
  ;   The returned position is an offset independent absolute value.}
  ;
  ; @usage
  ; (tag-starting-position "<div>My content</div>" [:div #"<div>" #"<\/div>"])
  ; =>
  ; 0
  ;
  ; @usage
  ; (tag-starting-position "<div><div></div></div>" [:div #"<div>" #"<\/div>"])
  ; =>
  ; 0
  ;
  ; @usage
  ; (tag-starting-position "</div> <div></div>" [:div #"<div>" #"<\/div>"])
  ; =>
  ; 7
  ;
  ; @return (integer)
  ([n searched-tag]
   (tag-starting-position n searched-tag {} {}))

  ([n searched-tag tags]
   (tag-starting-position n searched-tag tags {}))

  ([n [tag-name & _ :as searched-tag] tags options]
   (letfn [; ...
           (f0 [_ {:keys [cursor] :as state} {:keys [stop tag-starts?] :as metafunctions}]
               (if (tag-starts? tag-name)
                   (stop cursor)))]
          ; ...
          (let [tags (core.prototypes/tags-prototype tags)
                tags (vector/cons-item tags searched-tag)]
               (syntax-interpreter/interpreter n f0 nil tags options)))))

(defn tag-opening-position
  ; @links
  ; [cljc-syntax-interpreter](https://mt-devtools.github.io/cljc-syntax-interpreter)
  ;
  ; @description
  ; - Returns the opening position of the given searched tag in the given 'n' string.
  ; - By default, it ignores commented and quoted parts of the string.
  ;
  ; @param (string) n
  ; @param (vector) searched-tag
  ; [(keyword) tag-name
  ;  (regex pattern) pattern / opening-pattern
  ;  (regex pattern)(opt) closing-pattern
  ;  (map)(opt) options]
  ; @param (vectors in vector)(opt)(in decreasing priority order) tags
  ; [[(keyword) tag-name
  ;   (regex pattern) pattern / opening-pattern
  ;   (regex pattern)(opt) closing-pattern
  ;   (map)(opt) tag-options]]
  ; Default:
  ; [[:comment       #"\;"   #"\n" {:accepted-children []}]
  ;  [:meta-string   #"\^\"" #"\"" {:accepted-children []}]
  ;  [:regex-pattern #"\#\"" #"\"" {:accepted-children []}]
  ;  [:string        #"\""   #"\"" {:accepted-children []}]]
  ; @param (map)(opt) options
  ; {:endpoint (integer)(opt)
  ;   Quits searching at the given 'endpoint' position in the given 'n' string.
  ;  :offset (integer)(opt)
  ;   Starts searching at the given 'offset' position in the given 'n' string.
  ;   The returned position is an offset independent absolute value.}
  ;
  ; @usage
  ; (tag-opening-position "<div>My content</div>" [:div #"<div>" #"<\/div>"])
  ; =>
  ; 5
  ;
  ; @usage
  ; (tag-opening-position "<div><div></div></div>" [:div #"<div>" #"<\/div>"])
  ; =>
  ; 5
  ;
  ; @usage
  ; (tag-opening-position "</div> <div></div>" [:div #"<div>" #"<\/div>"])
  ; =>
  ; 12
  ;
  ; @return (integer)
  ([n searched-tag]
   (tag-opening-position n searched-tag {} {}))

  ([n searched-tag tags]
   (tag-opening-position n searched-tag tags {}))

  ([n [tag-name & _ :as searched-tag] tags options]
   (letfn [; ...
           (f0 [_ {:keys [cursor] :as state} {:keys [stop tag-opens?] :as metafunctions}]
               (if (tag-opens? tag-name)
                   (stop cursor)))]
          ; ...
          (let [tags (core.prototypes/tags-prototype tags)
                tags (vector/cons-item tags searched-tag)]
               (syntax-interpreter/interpreter n f0 nil tags options)))))

(defn tag-closing-position
  ; @links
  ; [cljc-syntax-interpreter](https://mt-devtools.github.io/cljc-syntax-interpreter)
  ;
  ; @description
  ; - Returns the closing position of the given searched tag in the given 'n' string.
  ; - By default, it ignores commented and quoted parts of the string.
  ;
  ; @param (string) n
  ; @param (vector) searched-tag
  ; [(keyword) tag-name
  ;  (regex pattern) pattern / opening-pattern
  ;  (regex pattern)(opt) closing-pattern
  ;  (map)(opt) options]
  ; @param (vectors in vector)(opt)(in decreasing priority order) tags
  ; [[(keyword) tag-name
  ;   (regex pattern) pattern / opening-pattern
  ;   (regex pattern)(opt) closing-pattern
  ;   (map)(opt) tag-options]]
  ; Default:
  ; [[:comment       #"\;"   #"\n" {:accepted-children []}]
  ;  [:meta-string   #"\^\"" #"\"" {:accepted-children []}]
  ;  [:regex-pattern #"\#\"" #"\"" {:accepted-children []}]
  ;  [:string        #"\""   #"\"" {:accepted-children []}]]
  ; @param (map)(opt) options
  ; {:endpoint (integer)(opt)
  ;   Quits searching at the given 'endpoint' position in the given 'n' string.
  ;  :offset (integer)(opt)
  ;   Starts searching at the given 'offset' position in the given 'n' string.
  ;   The returned position is an offset independent absolute value.}
  ;
  ; @usage
  ; (tag-closing-position "<div>My content</div>" [:div #"<div>" #"<\/div>"])
  ; =>
  ; 15
  ;
  ; @usage
  ; (tag-closing-position "<div><div></div></div>" [:div #"<div>" #"<\/div>"])
  ; =>
  ; 16
  ;
  ; @usage
  ; (tag-closing-position "</div> <div></div>" [:div #"<div>" #"<\/div>"])
  ; =>
  ; 12
  ;
  ; @return (integer)
  ([n searched-tag]
   (tag-closing-position n searched-tag {} {}))

  ([n searched-tag tags]
   (tag-closing-position n searched-tag tags {}))

  ([n [tag-name & _ :as searched-tag] tags options]
   (letfn [; ...
           (f0 [_ {:keys [cursor] :as state} {:keys [depth stop tag-closes?] :as metafunctions}]
               (if (and (tag-closes? tag-name)
                        (= 1 (depth)))
                   (stop cursor)))]
          ; ...
          (let [tags (core.prototypes/tags-prototype tags)
                tags (vector/cons-item tags searched-tag)]
               (syntax-interpreter/interpreter n f0 nil tags options)))))

(defn tag-ending-position
  ; @links
  ; [cljc-syntax-interpreter](https://mt-devtools.github.io/cljc-syntax-interpreter)
  ;
  ; @description
  ; - Returns the ending position of the given searched tag in the given 'n' string.
  ; - By default, it ignores commented and quoted parts of the string.
  ;
  ; @param (string) n
  ; @param (vector) searched-tag
  ; [(keyword) tag-name
  ;  (regex pattern) pattern / opening-pattern
  ;  (regex pattern)(opt) closing-pattern
  ;  (map)(opt) options]
  ; @param (vectors in vector)(opt)(in decreasing priority order) tags
  ; [[(keyword) tag-name
  ;   (regex pattern) pattern / opening-pattern
  ;   (regex pattern)(opt) closing-pattern
  ;   (map)(opt) tag-options]]
  ; Default:
  ; [[:comment       #"\;"   #"\n" {:accepted-children []}]
  ;  [:meta-string   #"\^\"" #"\"" {:accepted-children []}]
  ;  [:regex-pattern #"\#\"" #"\"" {:accepted-children []}]
  ;  [:string        #"\""   #"\"" {:accepted-children []}]]
  ; @param (map)(opt) options
  ; {:endpoint (integer)(opt)
  ;   Quits searching at the given 'endpoint' position in the given 'n' string.
  ;  :offset (integer)(opt)
  ;   Starts searching at the given 'offset' position in the given 'n' string.
  ;   The returned position is an offset independent absolute value.}
  ;
  ; @usage
  ; (tag-ending-position "<div>My content</div>" [:div #"<div>" #"<\/div>"])
  ; =>
  ; 21
  ;
  ; @usage
  ; (tag-ending-position "<div><div></div></div>" [:div #"<div>" #"<\/div>"])
  ; =>
  ; 22
  ;
  ; @usage
  ; (tag-ending-position "</div> <div></div>" [:div #"<div>" #"<\/div>"])
  ; =>
  ; 18
  ;
  ; @return (integer)
  ([n searched-tag]
   (tag-ending-position n searched-tag {} {}))

  ([n searched-tag tags]
   (tag-ending-position n searched-tag tags {}))

  ([n [tag-name & _ :as searched-tag] tags options]
   (letfn [; ...
           (f0 [_ {:keys [cursor] :as state} {:keys [depth stop tag-ends?] :as metafunctions}]
               (if (and (tag-ends? tag-name)
                        (= 0 (depth)))
                   (stop cursor)))]
          ; ...
          (let [tags (core.prototypes/tags-prototype tags)
                tags (vector/cons-item tags searched-tag)]
               (syntax-interpreter/interpreter n f0 nil tags options)))))
