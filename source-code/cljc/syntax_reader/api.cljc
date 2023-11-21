
(ns syntax-reader.api
    (:require [syntax-reader.core.check         :as core.check]
              [syntax-reader.core.config        :as core.config]
              [syntax-reader.grey-zones.engine  :as grey-zones.engine]
              [syntax-reader.interpreter.engine :as interpreter.engine]
              [syntax-reader.search.engine      :as search.engine]
              [syntax-reader.tags        :as tags]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

; syntax-reader.core.check
(def position-escaped? core.check/position-escaped?)

; syntax-reader.core.config
(def DEFAULT-TAGS core.config/DEFAULT-TAGS)

; syntax-reader.grey-zones.engine
(def grey-zones             grey-zones.engine/grey-zones)
(def remove-commented-parts grey-zones.engine/remove-commented-parts)

; syntax-reader.interpreter.engine
(def interpreter interpreter.engine/interpreter)

; syntax-reader.search.engine
(def first-position       search.engine/first-position)
(def opening-tag-position search.engine/opening-tag-position)
(def closing-tag-position search.engine/closing-tag-position)

; syntax-reader.tags
(def opening-brace-position   tags/opening-brace-position)
(def closing-brace-position   tags/closing-brace-position)
(def opening-bracket-position tags/opening-bracket-position)
(def closing-bracket-position tags/closing-bracket-position)
(def opening-paren-position   tags/opening-paren-position)
(def closing-paren-position   tags/closing-paren-position)
