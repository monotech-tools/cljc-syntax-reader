
(ns syntax-reader.tags.utils)

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn opening-match-found?
  ; @ignore
  ;
  ; @param (map) state
  ; @param (map) metafunctions
  ; {:tag-starts? (function)}
  ;
  ; @return (boolean)
  [_ {:keys [tag-starts?]}]
  (-> :$searched-tag tag-starts?))

(defn closing-match-found?
  ; @ignore
  ;
  ; @param (map) state
  ; {:metadata (map)
  ;   {:tag-initial-depth (integer)(opt)}}
  ; @param (map) metafunctions
  ; {:tag-depth (function)
  ;  :tag-closes? (function)}
  ;
  ; @return (boolean)
  [{{:keys [tag-initial-depth]} :metadata} {:keys [tag-depth tag-closes?]}]
  ; If the 'tag-initial-depth' value ...
  ; A) ... is NIL (only in the first iteration), it only acknowledges the closing pattern's found match at the actual cursor position
  ;        if it is closing the tag in depth of greater than 0. This condition ensures that the 'closing-match-position' function
  ;        doesn't return the position of a closing pattern's match that doesn't have a preceding opening pattern's match in the given 'n' string.
  ; B) ... is 0 (only in later iterations), it only acknowledges the closing pattern's found match at the actual cursor position if it is closing
  ;        the tag in depth of 1. This condition ensures that if the searching started outside of the tag, the 'closing-match-position'
  ;        function returns the position of a closing pattern's match that closes the first opened tag proceeded where the search started.
  ; C) ... is greater than 0 (only in later iterations), it acknowledges the closing pattern's found match at the actual cursor position if it is
  ;        closing the tag in the same depth as it was when the searching started.
  ; E.g., (closing-match-position "abc(def(ghi))" #"\(" #"\)" {} {:offset 11}) => 11 (A, first iteration started right before a closing tag)
  ;       (closing-match-position "abc(def(ghi))" #"\(" #"\)" {} {:offset 12}) => 12 (A, first iteration started right before a closing tag)
  ;       (closing-match-position "abc(def(ghi))" #"\(" #"\)" {} {:offset  0}) => 12 (B, first iteration started outside of the tag)
  ;       (closing-match-position "abc(def(ghi))" #"\(" #"\)" {} {:offset  5}) => 12 (C, first iteration started within the tag at depth 1)
  ;       (closing-match-position "abc(def(ghi))" #"\(" #"\)" {} {:offset  9}) => 11 (C, first iteration started within the tag at depth 2)
  (cond (-> tag-initial-depth nil?)  (and (-> :$searched-tag tag-closes?)
                                          (-> :$searched-tag tag-depth (> 0)))
        (-> tag-initial-depth zero?) (and (-> :$searched-tag tag-closes?)
                                          (-> :$searched-tag tag-depth (= 1)))
        :tag-initial-depth>0         (and (-> :$searched-tag tag-closes?)
                                          (-> :$searched-tag tag-depth (= tag-initial-depth)))))

(defn first-iteration?
  ; @ignore
  ;
  ; @param (map) state
  ; {:metadata (map)
  ;   {:tag-initial-depth (integer)(opt)}}
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [{{:keys [tag-initial-depth]} :metadata} _]
  (-> tag-initial-depth nil?))

(defn init-metadata
  ; @ignore
  ;
  ; @param (map) state
  ; @param (map) metafunctions
  ; {:set-metadata (function)
  ;  :tag-depth (function)}
  ;
  ; @return (boolean)
  [_ {:keys [set-metadata tag-depth]}]
  (let [tag-initial-depth (tag-depth :$searched-tag)]
       (set-metadata {:tag-initial-depth tag-initial-depth})))
