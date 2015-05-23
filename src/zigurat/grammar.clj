(ns zigurat.grammar
  "Rename to zigurat.lang || z.defphrase or
  keep it open to other grammar categories ?"

  (:require [zigurat.graph :refer [graph-reducer ->Part]]))

;; ---------------------------
;;
;; Defphrase: move to own ns.
;;
;; ===========================

;;
;; Method Code Factory
;;

(def make-phrase-tag
  (comp (partial keyword "zigurat.grammar") clojure.string/upper-case name))

(defn make-method-header-factory
  [mmname]
  (fn
    [code]
    (concat `(defmethod ~mmname) code)))

(defn make-method-body-factory
  [phrase-tag]
  (fn method-body-factory
    ([args body]
     (method-body-factory args body []))
    ([args body ctx]
     (let [method-body `(->Part ~phrase-tag ~(reduce graph-reducer nil body))
           types (vec (map make-phrase-tag args))]
       (if (seq ctx)
         `(~types
           ~args
           (let ~ctx
             ~method-body))
         `(~types
           ~args
           ~method-body))))))

(defn make-method-factory
  [mm-symbol phrase-tag]
  (comp (make-method-header-factory mm-symbol) (make-method-body-factory phrase-tag)))

(defn make-mdefinitions
  [mm-symbol phrase-tag forms]
  (map (partial apply (make-method-factory mm-symbol phrase-tag)) forms))

;;
;; Defphrase Macro
;;

(defn mm-dispatcher [& elems] (vec (map :tag elems)))

(defmacro defphrase
  [pname docstr & forms]
  (let [phrase-tag    (make-phrase-tag pname)
        docstr?       (string? docstr)
        mm-symbol     (symbol (clojure.string/lower-case (name pname)))
        mm-sym-docstr (if docstr? (list mm-symbol docstr) (list mm-symbol))
        m-forms       (if docstr? forms (cons docstr forms))
        m-definitions (make-mdefinitions mm-symbol phrase-tag m-forms)]
    `(do
       (defmulti ~@mm-sym-docstr ~mm-dispatcher)
       ~@m-definitions)))
