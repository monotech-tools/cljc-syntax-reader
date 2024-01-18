
(ns syntax-reader.api
    (:require [syntax-reader.read.engine     :as read.engine]
              [syntax-reader.remove.engine   :as remove.engine]
              [syntax-reader.search.defaults :as search.defaults]
              [syntax-reader.search.engine   :as search.engine]
              [syntax-reader.update.engine   :as update.engine]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

; @redirect (syntax-reader.read.engine/*)
(def read-tags read.engine/read-tags)

; @redirect (syntax-reader.remove.engine/*)
(def remove-tags remove.engine/remove-tags)

; @redirect (syntax-reader.search.defaults/*)
(def brace-starting-position   search.defaults/brace-starting-position)
(def brace-opening-position    search.defaults/brace-opening-position)
(def brace-closing-position    search.defaults/brace-closing-position)
(def brace-ending-position     search.defaults/brace-ending-position)
(def bracket-starting-position search.defaults/bracket-starting-position)
(def bracket-opening-position  search.defaults/bracket-opening-position)
(def bracket-closing-position  search.defaults/bracket-closing-position)
(def bracket-ending-position   search.defaults/bracket-ending-position)
(def paren-starting-position   search.defaults/paren-starting-position)
(def paren-opening-position    search.defaults/paren-opening-position)
(def paren-closing-position    search.defaults/paren-closing-position)
(def paren-ending-position     search.defaults/paren-ending-position)

; @redirect (syntax-reader.search.engine/*)
(def tag-starting-position search.engine/tag-starting-position)
(def tag-opening-position  search.engine/tag-opening-position)
(def tag-closing-position  search.engine/tag-closing-position)
(def tag-ending-position   search.engine/tag-ending-position)

; @redirect (syntax-reader.update.engine/*)
(def update-tags update.engine/update-tags)
