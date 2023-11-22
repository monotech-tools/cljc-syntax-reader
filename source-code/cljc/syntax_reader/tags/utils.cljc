
(ns syntax-reader.tags.utils)

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn opening-tag-found?
  ; @ignore
  ;
  ; @param (map) state
  ; @param (map) metafunctions
  ; {:opening-tag-starts? (function)}
  ;
  ; @return (boolean)
  [_ {:keys [opening-tag-starts?]}]
  (-> :$searched-tag opening-tag-starts?))

(defn closing-tag-found?
  ; @ignore
  ;
  ; @param (map) state
  ; {:tag-initial-depth (integer)(opt)}
  ; @param (map) metafunctions
  ; {:closing-tag-starts? (function)
  ;  :tag-actual-depth (function)}
  ;
  ; @return (boolean)
  [{:keys [tag-initial-depth]} {:keys [tag-actual-depth tag-closes?]}]
  ; If the 'tag-initial-depth' value ...
  ; A) ... is NIL (only in the first iteration), it only acknowledges a found closing tag at the actual cursor position
  ;        if it is closing the tag in depth greater than 0. This condition ensures that the 'closing-tag-position' function
  ;        doesn't return the position of a closing tag that doesn't have a preceeding opening tag in the given 'n' string.
  ; B) ... is 0 (only in later iterations), it only acknowledges a found closing tag at the actual cursor position if it is closing
  ;        the tag in depth of 1. This condition ensures that if the searching started outside of the tag, the 'closing-tag-position'
  ;        function returns the position of a closing tag that closes the first opened tag proceeded where the search started.
  ; C) ... is greater than 0 (only in later iterations), it acknowledges a found closing tag at the actual cursor position if it is closing
  ;        the tag in the same depth as it started the searching.
  ; E.g., (closing-tag-position "abc(def(ghi))" #"\(" #"\)" {} {:offset 11}) => 11 (A, first iteration started right before a closing tag)
  ;       (closing-tag-position "abc(def(ghi))" #"\(" #"\)" {} {:offset 12}) => 12 (A, first iteration started right before a closing tag)
  ;       (closing-tag-position "abc(def(ghi))" #"\(" #"\)" {} {:offset  0}) => 12 (B, first iteration started outside of the tag)
  ;       (closing-tag-position "abc(def(ghi))" #"\(" #"\)" {} {:offset  5}) => 12 (C, first iteration started within the tag at depth 1)
  ;       (closing-tag-position "abc(def(ghi))" #"\(" #"\)" {} {:offset  9}) => 11 (C, first iteration started within the tag at depth 2)
  (cond (-> tag-initial-depth nil?)  (and (-> :$searched-tag tag-closes?)
                                          (-> :$searched-tag tag-actual-depth (> 0)))
        (-> tag-initial-depth zero?) (and (-> :$searched-tag tag-closes?)
                                          (-> :$searched-tag tag-actual-depth (= 1)))
        :tag-initial-depth>0         (and (-> :$searched-tag tag-closes?)
                                          (-> :$searched-tag tag-actual-depth (= tag-initial-depth)))))

(defn first-iteration?
  ; @ignore
  ;
  ; @param (map) state
  ; {:tag-initial-depth (integer)(opt)}
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [{:keys [tag-initial-depth]} _]
  (-> tag-initial-depth nil?))

(defn init-state
  ; @ignore
  ;
  ; @param (map) state
  ; @param (map) metafunctions
  ; {:set-state (function)
  ;  :tag-actual-depth (function)}
  ;
  ; @return (boolean)
  [_ {:keys [set-state tag-actual-depth]}]
  (let [tag-initial-depth (tag-actual-depth :$searched-tag)]
       (set-state {:tag-initial-depth tag-initial-depth})))
