
(ns syntax-reader.api
    (:require [syntax-reader.core.check       :as core.check]
              [syntax-reader.core.config      :as core.config]
              [syntax-reader.grey-zones  :as grey-zones]
              [syntax-reader.interpreter.core :as interpreter.core]
              [syntax-reader.search      :as search]
              [syntax-reader.tags        :as tags]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

; syntax-reader.core.check
(def position-escaped? core.check/position-escaped?)

; syntax-reader.core.config
(def DEFAULT-TAGS core.config/DEFAULT-TAGS)

; syntax-reader.grey-zones
(def grey-zones             grey-zones/grey-zones)
(def remove-commented-parts grey-zones/remove-commented-parts)

; syntax-reader.interpreter.core
(def interpreter interpreter.core/interpreter)

; syntax-reader.search
(def first-position       search/first-position)
(def opening-tag-position search/opening-tag-position)
(def closing-tag-position search/closing-tag-position)

; syntax-reader.tags
(def opening-brace-position   tags/opening-brace-position)
(def closing-brace-position   tags/closing-brace-position)
(def opening-bracket-position tags/opening-bracket-position)
(def closing-bracket-position tags/closing-bracket-position)
(def opening-paren-position   tags/opening-paren-position)
(def closing-paren-position   tags/closing-paren-position)
