
(ns syntax-reader.config)

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

; @constant (vectors in map)
(def DEFAULT-TAGS {:brace   ["{" "}"]
                   :bracket ["[" "]"]
                   :comment [";" "\n" {:disable-interpreter? true}]
                   :paren   ["(" ")"]
                   :quote   ["\"" "\"" {:disable-interpreter? true}]})
