
(ns syntax-reader.interpreter.metafunctions
    (:require [syntax-reader.interpreter.utils :as interpreter.utils]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn interpreter-disabled-f
  ; @ignore
  ;
  ; @description
  ; Returns the 'interpreter-disabled?' metafunction.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ;
  ; @return (function)
  [n tags options state]
  ; @description
  ; Returns TRUE if the interpreter is disabled by an opened tag.
  ;
  ; @usage
  ; (interpreter-disabled?)
  ;
  ; @return (boolean)
  (fn [] (-> (interpreter.utils/interpreter-disabled-by n tags options state) some?)))

(defn interpreter-enabled-f
  ; @ignore
  ;
  ; @description
  ; Returns the 'interpreter-enabled?' metafunction.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ;
  ; @return (function)
  [n tags options state]
  ; @description
  ; Returns TRUE if the interpreter is NOT disabled by an opened tag.
  ;
  ; @usage
  ; (interpreter-enabled?)
  ;
  ; @return (boolean)
  (fn [] (-> (interpreter.utils/interpreter-disabled-by n tags options state) not)))

(defn stop-f
  ; @ignore
  ;
  ; @description
  ; Returns the 'stop' metafunction.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ;
  ; @return (function)
  [n tags options state]
  ; @description
  ; Stops the interpreter immediatelly and it returns the parameter of this ('stop') function as interpreter output.
  ;
  ; @param (*) result
  ;
  ; @usage
  ; (stop "My output")
  ;
  ; @return (vector)
  ; [(namespaced keyword) stop-marker
  ;  (*) result]
  (fn [result] [:$stop result]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn parent-tag-name-f
  ; @ignore
  ;
  ; @description
  ; Returns the 'parent-tag-name' metafunction.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ;
  ; @return (function)
  [n tags options state]
  (fn [] (-> state interpreter.utils/innermost-tag :name)))

(defn tag-actual-depth-f
  ; @ignore
  ;
  ; @description
  ; Returns the 'tag-actual-depth' metafunction.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ;
  ; @return (function)
  [n tags options state]
  ; @description
  ; Returns the depth of the given tag at the actual cursor position.
  ;
  ; @param (keyword) tag-name
  ;
  ; @usage
  ; (tag-actual-depth :my-tag)
  ;
  ; @return (integer)
  (fn [tag-name] (interpreter.utils/tag-actual-depth n tags options state tag-name)))

(defn tag-opened-f
  ; @ignore
  ;
  ; @description
  ; Returns the 'tag-opened?' metafunction.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ;
  ; @return (function)
  [n tags options state]
  ; @description
  ; Returns whether the given tag is opened at the actual cursor position in any depth.
  ;
  ; @param (keyword) tag-name
  ;
  ; @usage
  ; (tag-opened? :my-tag)
  ;
  ; @return (boolean)
  (fn [tag-name] (interpreter.utils/tag-opened? n tags options state tag-name)))

(defn tag-not-opened-f
  ; @ignore
  ;
  ; @description
  ; Returns the 'tag-not-opened?' metafunction.
  ;
  ; @param (string) n
  ; @param (map) tags
  ; @param (map) options
  ; @param (map) state
  ;
  ; @return (function)
  [n tags options state]
  ; @description
  ; Returns whether the given tag is not opened at the actual cursor position in any depth.
  ;
  ; @param (keyword) tag-name
  ;
  ; @usage
  ; (tag-not-opened? :my-tag)
  ;
  ; @return (boolean)
  (fn [tag-name] (-> (interpreter.utils/tag-opened? n tags options state tag-name) not)))

(defn tag-opened-at-f
  ; @ignore
   ; @description
   ; Returns the 'tag-opened-at' metafunction.
   ;
   ; @param (string) n
   ; @param (map) tags
   ; @param (map) options
   ; @param (map) state
   ;
   ; @return (function)
   [n tags options state]
   ; @description
   ; Returns the cursor position where the tag's opening tag ended.
   ;
   ; @param (keyword) tag-name
   ;
   ; @usage
   ; (tag-opened-at :my-tag)
   ;
   ; @return (integer)
   (fn [tag-name] (interpreter.utils/tag-opened-at n tags options state tag-name)))

(defn tag-started-at-f
  ; @ignore
   ; @description
   ; Returns the 'tag-started-at' metafunction.
   ;
   ; @param (string) n
   ; @param (map) tags
   ; @param (map) options
   ; @param (map) state
   ;
   ; @return (function)
   [n tags options state]
   ; @description
   ; Returns the cursor position where the tag's opening tag started.
   ;
   ; @param (keyword) tag-name
   ;
   ; @usage
   ; (tag-started-at :my-tag)
   ;
   ; @return (integer)
   (fn [tag-name] (interpreter.utils/tag-started-at n tags options state tag-name)))

(defn opening-tag-starts-f
  ; @ignore
   ; @description
   ; Returns the 'opening-tag-starts?' metafunction.
   ;
   ; @param (string) n
   ; @param (map) tags
   ; @param (map) options
   ; @param (map) state
   ;
   ; @return (function)
   [n tags options state]
   ; @description
   ; Returns whether the given tag's opening tag starts at the actual cursor position.
   ;
   ; @param (keyword) tag-name
   ;
   ; @usage
   ; (opening-tag-starts? :my-tag)
   ;
   ; @return (boolean)
   (fn [tag-name] (if-not (interpreter.utils/interpreter-disabled? n tags options state)
                          (interpreter.utils/opening-tag-starts?   n tags options state tag-name))))

(defn opening-tag-ends-f
  ; @ignore
   ; @description
   ; Returns the 'opening-tag-ends?' metafunction.
   ;
   ; @param (string) n
   ; @param (map) tags
   ; @param (map) options
   ; @param (map) state
   ;
   ; @return (function)
   [n tags options state]
   ; @description
   ; Returns whether the given tag's opening tag ends at the actual cursor position.
   ;
   ; @param (keyword) tag-name
   ;
   ; @usage
   ; (opening-tag-ends? :my-tag)
   ;
   ; @return (boolean)
   (fn [tag-name] (if-not (interpreter.utils/interpreter-disabled? n tags options state)
                          (interpreter.utils/opening-tag-ends?     n tags options state tag-name))))

(defn closing-tag-starts-f
  ; @ignore
   ; @description
   ; Returns the 'closing-tag-starts?' metafunction.
   ;
   ; @param (string) n
   ; @param (map) tags
   ; @param (map) options
   ; @param (map) state
   ;
   ; @return (function)
   [n tags options state]
   ; @description
   ; Returns whether the given tag's closing tag starts at the actual cursor position.
   ;
   ; @param (keyword) tag-name
   ;
   ; @usage
   ; (closing-tag-starts? :my-tag)
   ;
   ; @return (boolean)
   (fn [tag-name] (if-not (interpreter.utils/interpreter-disabled? n tags options state)
                          (interpreter.utils/closing-tag-starts?   n tags options state tag-name))))

(defn closing-tag-ends-f
  ; @ignore
   ; @description
   ; Returns the 'closing-tag-ends?' metafunction.
   ;
   ; @param (string) n
   ; @param (map) tags
   ; @param (map) options
   ; @param (map) state
   ;
   ; @return (function)
   [n tags options state]
   ; @description
   ; Returns whether the given tag's closing tag ends at the actual cursor position.
   ;
   ; @param (keyword) tag-name
   ;
   ; @usage
   ; (closing-tag-ends? :my-tag)
   ;
   ; @return (boolean)
   (fn [tag-name] (if-not (interpreter.utils/interpreter-disabled? n tags options state)
                          (interpreter.utils/closing-tag-ends?     n tags options state tag-name))))
