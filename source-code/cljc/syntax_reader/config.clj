
(ns syntax-reader.config)

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

; @constant (string pairs in vectors in map)
(def DEFAULT-TAGS {:brace   ["{" "}"]
                   :bracket ["[" "]"]
                   :comment [";" "\n"]
                   :paren   ["(" ")"]
                   :quote   ["\"" "\""]})
