
(ns syntax-reader.utils)

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn default-tags
  [])
  


(defn default-comment-tags
  ; @description
  ; - Sets the default comment opening and closing tag values in the given 'tags' map.
  ; - Removes the closing tag value in case of the opening and closing tag values are the same.
  ;   This is a necessarry action because the 'tag-positions' function doesn't collect duplicated tag occurences!
  ;
  ; @param (map) tags
  ; {:comment-close-tag (string)(opt)
  ;  :comment-open-tag (string)(opt)}
  ;
  ; @usage
  ; (default-comment-tags {...})
  ;
  ; @return (map)
  ; {:comment-close-tag (string)
  ;  :comment-open-tag (string)}
  [tags]
  (cond-> tags ; Uses the ";" string as the default comment opening tag.
               (-> tags :comment-open-tag  nil?) (assoc :comment-open-tag  ";")
               ; Uses the "\n" string as the default comment closing tag.
               (-> tags :comment-close-tag nil?) (assoc :comment-close-tag "\n")
               ; Removes the comment closing tag if it is the same as the opening tag
               ; (except they are both NILs in the given 'tags' map: NIL values are overwritten with default values).
               (and (-> tags :comment-open-tag some?)
                    (= (:comment-close-tag tags)
                       (:comment-open-tag  tags)))
               (dissoc :comment-close-tag))

  (merge {:comment-open-tag ";"
          :comment-close-tag "\n"}
         (-> tags)))

(defn default-quote-tags
  ; @description
  ; - Sets the default quote opening and closing tag values in the given 'tags' map.
  ; - Removes the closing tag value in case of the opening and closing tag values are the same.
  ;   This is a necessarry action because the 'tag-positions' function doesn't collect duplicated tag occurences!
  ;
  ; @param (map) tags
  ; {:quote-close-tag (string)(opt)
  ;  :quote-open-tag (string)(opt)}
  ;
  ; @usage
  ; (default-quote-tags {...})
  ;
  ; @return (map)
  ; {:quote-close-tag (string)
  ;  :quote-open-tag (string)}
  [tags]
  (cond-> tags ; Uses the "\"" string as the default quote opening tag.
               (-> tags :quote-open-tag nil?) (assoc :quote-open-tag  "\"")
               ; Removes the quote closing tag if it is the same as the opening tag
               ; (except they are both NILs in the given 'tags' map: NIL values are overwritten with default values).
               (and (-> tags :quote-open-tag some?)
                    (= (:quote-close-tag tags)
                       (:quote-open-tag  tags)))
               (dissoc :quote-close-tag))

  (merge {:quote-open-tag "\""
          :quote-close-tag "\""}
         (-> tags)))
