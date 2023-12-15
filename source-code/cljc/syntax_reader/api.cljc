
(ns syntax-reader.api
    (:require [syntax-reader.read.engine   :as read.engine]
              [syntax-reader.remove.engine :as remove.engine]
              [syntax-reader.search.engine :as search.engine]
              [syntax-reader.tags.defaults :as tags.defaults]
              [syntax-reader.tags.engine   :as tags.engine]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

; @redirect (syntax-reader.remove.engine)
(def remove-tags remove.engine/remove-tags)

; @redirect (syntax-reader.read.engine)
(def read-tags read.engine/read-tags)

; @redirect (syntax-reader.search.engine)
(def first-position search.engine/first-position)

; @redirect (syntax-reader.tags.defaults)
(def opening-brace-position   tags.defaults/opening-brace-position)
(def closing-brace-position   tags.defaults/closing-brace-position)
(def opening-bracket-position tags.defaults/opening-bracket-position)
(def closing-bracket-position tags.defaults/closing-bracket-position)
(def opening-paren-position   tags.defaults/opening-paren-position)
(def closing-paren-position   tags.defaults/closing-paren-position)

; @redirect (syntax-reader.tags.engine)
(def opening-match-position tags.engine/opening-match-position)
(def closing-match-position tags.engine/closing-match-position)
