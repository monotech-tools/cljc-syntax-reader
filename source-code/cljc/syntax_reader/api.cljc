
(ns syntax-reader.api
    (:require [syntax-reader.check       :as check]
              [syntax-reader.comment     :as comment]
              [syntax.interpreter :as interpreter]
              [syntax.tags        :as tags]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

; syntax.check
(def position-escaped?   check/position-escaped?)
(def position-commented? check/position-commented?)
(def position-quoted?    check/position-quoted?)

; syntax-reader.comment
(def remove-commented-zones comment/remove-commented-zones)

; syntax-reader.interpreter
(def tag-positions interpreter/tag-positions)
(def grey-zones    interpreter/grey-zones)

; syntax-reader.tags
(def tag-position           tags/tag-position)
(def tag-count              tags/tag-count)
(def tags-balanced?         tags/tags-balanced?)
(def open-tag-position      tags/open-tag-position)
(def close-tag-position     tags/close-tag-position)
(def open-brace-position    tags/open-brace-position)
(def close-brace-position   tags/close-brace-position)
(def open-bracket-position  tags/open-bracket-position)
(def close-bracket-position tags/close-bracket-position)
(def open-paren-position    tags/open-paren-position)
(def close-paren-position   tags/close-paren-position)
