
(ns syntax-reader.api
    (:require [syntax-reader.check       :as check]
              [syntax-reader.comment     :as comment]
              [syntax-reader.interpreter :as interpreter]
              [syntax-reader.tags        :as tags]
              [syntax-reader.utils       :as utils]))

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
(def tag-first-position       tags/tag-first-position)
(def tag-count                tags/tag-count)
(def tags-balanced?           tags/tags-balanced?)
(def tag-opening-position     tags/tag-opening-position)
(def tag-closing-position     tags/tag-closing-position)
(def brace-opening-position   tags/brace-opening-position)
(def brace-closing-position   tags/brace-closing-position)
(def bracket-opening-position tags/bracket-opening-position)
(def bracket-closing-position tags/bracket-closing-position)
(def paren-opening-position   tags/paren-opening-position)
(def paren-closing-position   tags/paren-closing-position)

; syntax-reader.utils
(def default-comment-tags utils/default-comment-tags)
(def default-quote-tags   utils/default-quote-tags)
