(ns clojure-carp
  (:require [clojure.string :as str]
            [clojure.tools.logging :as lgr]
            [clojure.set :as set])
  (:import (java.lang Exception Class)))


;; *******************
;; BEG: atom functions
;; *******************

(defn atom-swapper-one-arg [current-value new-value] new-value)

(defn update-atom-one-arg
  [atom-instance new-value]
  (swap! atom-instance
         atom-swapper-one-arg
         new-value))

(defn maybe-update-atom-one-arg
  [atom-instance new-value]
  (if new-value (update-atom-one-arg atom-instance new-value)))

;; *******************
;; FIN: atom functions
;; *******************

;; *************************
;; BEG: internal diagnostics
;; *************************

;;(def carp-diagnostics-status (atom true))
(def carp-diagnostics-status (atom false))

(defn set-diagnostics [value] (swap! carp-diagnostics-status atom-swapper-one-arg value))

(defmacro macro-set-diagnostics [value] (set-diagnostics value) nil)

(defn carp-diagnostic-format-value [value] (format ">%s< >%s<" (class value) value))

(defn carp-diagnostic
  [& args]
  (if @carp-diagnostics-status (doall (apply println args))))


;; *************************
;; FIN: internal diagnostics
;; *************************

;; *************************************
;; BEG: misc exception support functions
;; *************************************

(defn format-stringify-args
  [& args]
  (let [normalised-args (flatten args)]
    #_(doall (for [arg normalised-args] (doall (println (format "format-stringify-args ARG ^%s^ ^%s^" (class arg) arg)))))
    (apply str (interpose " " normalised-args))))

(defn format-exception-value [value] (str "Value " (carp-diagnostic-format-value value)))
(defn format-assertion-value [value] (str "Value " (carp-diagnostic-format-value value)))

;; *************************************
;; FIN: misc exception support functions
;; *************************************


;; ***************
;; BEG: exceptions
;; ***************


(defn raise-exception
  "throw an exception"
  [& args]
  (let [exception-message (format-stringify-args args)]
    (doall (println "RAISE EXCEPTION" exception-message))
    (lgr/fatal exception-message)
    (throw (Exception. exception-message))))


(defn surprise-exception
  [value & args]
  (raise-exception "Surprise Exception" (format-exception-value value) args))


(defn missing-exception
  [value & args]
  (raise-exception "Missing Exception" (format-exception-value value) args))


(defn duplicate-exception
  [value & args]
  (raise-exception "Duplicate Exception" (format-exception-value value) args))


(defn trace-exception
  [value & args]
  (raise-exception "Trace Exception" (format-exception-value value) args))


(defn logic-exception
  [value & args]
  (raise-exception "Logic Exception" (format-exception-value value) args))


(defn contract-exception
  [value & args]
  (raise-exception "Contract Exception" (format-exception-value value) args))


;; ***************
;; FIN: exceptions
;; ***************

;; ***************
;; BEG: assertions
;; ***************

(defn report-assertion-failure
  "throw an assertion-failure"
  [& args]
  (let [assertion-failure-message (format-stringify-args args)]
    (doall (println "Assertion failure" assertion-failure-message))
    (lgr/fatal assertion-failure-message)
    nil))

(defn report-contract-failure
  [value & args]
  (report-assertion-failure "Contract Failure" (format-assertion-value value) args))

;; ***************
;; FIN: assertions
;; ***************

;; ***************************
;; BEG: misc support functions
;; ***************************

(defn format-stringify-args
  [& args]
  (let [normalised-args (flatten args)]
    #_(doall (for [arg normalised-args] (doall (println (format "format-stringify-args ARG ^%s^ ^%s^" (class arg) arg)))))
    (apply str (interpose " " normalised-args))))

(defn to-collection
  [any]
  (cond
   (coll? any) any
   :else (list any)))

(defn to-vector
  [any]
  (cond
   (vector? any) any
   :else [any]))

(defn resolve-name-from-keyword
  [name-keyword]
  {:pre [(keyword? name-keyword)] :post [(string? %)]}
  (-> name-keyword str (subs 1)))

(defn resolve-name-from-args
  [& args]
  {:post [(string? %)]}
  (apply str (flatten args)))

(defn resolve-name-from-any
  "create a name from anything
   names are strings
   e.g. for keywords stringifies but removes leading colon"
  [any]
  (cond
   (nil? any) (surprise-exception :any "resolve-name-from-any" "any is nil")
   (keyword? any) (resolve-name-from-keyword any)
   :else (str any)))

(defn resolve-symbol-from-any
  [any]
  {:post [(symbol? %)]}
  (cond
   (symbol? any) any
   :else (symbol (resolve-name-from-any any))))


(defn resolve-any-class-telltale
  [any]
  (cond
   (nil? any) "nil"
   :else (first  (re-find #"(\p{Upper}\S+)?+$" (str (class any))))))

(defn resolve-symbol-from-args
  [& args]
  {:post [(symbol? %)]}
  (symbol (resolve-name-from-args args)))

;;(re-find #"(\p{Upper}\S+)?+$" s1)

;; ***************************
;; FIN: misc support functions
;; ***************************


;; **************
;; BEG: manifests
;; **************

(def manifest-namer-form '(fn [x] (str "manifest-" (clojure-carp/resolve-name-from-any x))))
(def manifest-namer-fn (fn [x] (str "manifest-" (clojure-carp/resolve-name-from-any x)))) 

(def default-source-namer manifest-namer-fn) ;; called by other fns

(def manifest-spec-key-source :source)
(def manifest-spec-key-source-namer :source-namer) ;; the key to use if passed

(def manifest-spec-key-target :target)
(def manifest-spec-key-target-namer :target-namer) ;; the key to use if passed

(def manifest-ctrl-key-source manifest-spec-key-source)
(def manifest-ctrl-key-target manifest-spec-key-target)
(def manifest-ctrl-key-name :name)
(def manifest-ctrl-key-form :form)
(def manifest-ctrl-key-def :def)

(def default-manifest-ctrl {manifest-ctrl-key-target {manifest-ctrl-key-form  manifest-namer-form}})

(def manifest-spec-to-ctrl-mapping
  {manifest-spec-key-source {manifest-ctrl-key-form manifest-spec-key-source-namer}
   manifest-spec-key-target {manifest-ctrl-key-form manifest-spec-key-target-namer}})

(defn validate-manifest-vector
  [manifest-spec]
  {:pre [(vector? manifest-spec)  ] :post [(vector? %)]}
  (or  (= 2 (count manifest-spec)) (surprise-exception manifest-spec "manifest-spec wrong size - must be 2"))
  (for [manifest-entry manifest-spec]
    (or (keyword? manifest-entry) (symbol? manifest-entry) (surprise-exception manifest-entry "manifest-entry not keyword or symbol")))
  manifest-spec)

(defn normalise-manifest-spec-to-map
  [manifest-spec]
  {:post [(map? %)]}
  (carp-diagnostic "normalise-manifest-spec-to-map" "ENTR" "MANIFEST-SPEC" (carp-diagnostic-format-value  manifest-spec))
  (let [manifest-spec-vector
        (cond
         (keyword? manifest-spec) [manifest-spec manifest-spec]
         (vector? manifest-spec) manifest-spec
         :else (surprise-exception manifest-spec "manifest-spec is wat?"))

        _ (validate-manifest-vector manifest-spec-vector)
        
        manifest-norm {manifest-spec-key-source (first manifest-spec-vector)  manifest-spec-key-target (second manifest-spec-vector)}]
    manifest-norm))

(defn- define-manifest-resolve-namer-var
  [namer]
  (let [namer# namer
        namer-var#
        (cond
         (nil? namer#) nil
         (symbol? namer#) (var-get (resolve namer#))
         :else (surprise-exception namer# "define-manifest-resolve-namer-var" "namer is wat?")
         ;;  :else (var-get (resolve namer#)) ;; see what happends

         )]
    namer-var#))

(defn define-manifest-form
  [manifest-ctrl manifest-norm]
  {:pre [(map? manifest-ctrl) (map? manifest-norm)] :post [(coll? %)]}
  (let [fn-source-namer# (get-in manifest-ctrl [manifest-ctrl-key-source manifest-ctrl-key-name])
        fn-target-namer# (or (get-in manifest-ctrl [manifest-ctrl-key-target manifest-ctrl-key-name]) (surprise-exception manifest-spec-key-target-namer  "no target namer fn"))

        fn-source-namer-var# (define-manifest-resolve-namer-var fn-source-namer#)
        fn-target-namer-var# (define-manifest-resolve-namer-var fn-target-namer#)
        
        source-nom# (get manifest-norm manifest-spec-key-source)
        source-nrm# (if fn-source-namer-var# (fn-source-namer-var# source-nom#) source-nom#)

        target-nom# (get manifest-norm manifest-spec-key-target)
        target-nrm# (cond
                     (symbol? target-nom#) target-nom# ;; leave it alone
                     :else (resolve-symbol-from-any (if fn-target-namer-var# (fn-target-namer-var# target-nom#) target-nom#)))

        target-sym# (cond
                     (symbol? target-nrm#) target-nrm#
                     (list? target-nrm#) target-nrm#
                     (instance? clojure.lang.Cons target-nrm#) target-nrm#
                     (string? target-nrm#) (symbol target-nrm#)
                     :else (surprise-exception target-nrm# "define-manifest-form" "target-nrm# not string, list or symbol"))

        manifest-form# `(def ~target-sym# ~source-nrm#)]
    manifest-form#))

(defmacro define-manifest-forms
  [manifest-ctrl & args]
  (let [manifests# args
        manifest-forms#
        (into {}
              (for [manifest# manifests#]
                (let [manifest-norm# (normalise-manifest-spec-to-map manifest#)
                      manifest-form# (define-manifest-form manifest-ctrl manifest-norm#)]
                  [manifest# manifest-form#])))]
    `(do
       ~@(for [[_ def#] manifest-forms#] def#))))

(defn resolve-manifest-ctrl
  ([ctrl-spec] (resolve-manifest-ctrl ctrl-spec {}))
  ([ctrl-spec base-spec]
     {:pre [(map? ctrl-spec) (map? base-spec)] :post [(map? %)]}
     (let [spec-to-ctrl-mapping# manifest-spec-to-ctrl-mapping
                     rslv-spec#
           (into {}
                 (for [[mapper-key#  mapper-map#] spec-to-ctrl-mapping#]
                    (do
                      (let [mapper-spec# (get base-spec mapper-key# {})
                            work-spec#
                            (merge mapper-spec#
                                   (reduce (fn [s [tgt-key src-key]]
                                             (when-let [src-value# (get ctrl-spec src-key)] (assoc s tgt-key src-value#)))
                                           {}
                                           mapper-map#))

                            work-form# (get work-spec# manifest-ctrl-key-form)

                            name-spec# (cond
                                       (nil? work-form#) nil
                                       (symbol? work-form#) {manifest-ctrl-key-name work-form#}

                                       ;; if a coll assume a fn deinition
                                       (coll? work-form#)
                                       (let [ctrl-name# (gensym (str "fn-" (resolve-name-from-keyword mapper-key#) "-namer"))
                                             ctrl-def# `(def ~ctrl-name# ~work-form#)]
                                         {manifest-ctrl-key-name ctrl-name#
                                          manifest-ctrl-key-def ctrl-def#})

                                       :else (surprise-exception work-form# "resolve-manifest-ctrl" "work-form is wat?"))

                            done-spec# (merge work-spec# name-spec#)]

                        (if done-spec# [mapper-key# done-spec#])))))]

        rslv-spec#)))

(defmacro define-manifests
  [& args]
  (let [
        manifest-first (first args)
        manifest-args (cond
                       (map? manifest-first) (rest args)
                       (keyword? manifest-first) args
                       (vector? manifest-first) args
                       :else (surprise-exception manifest-first "define-manifests" "manifest-first is what?"))

        ctrl-nrm# (if (map? manifest-first)
                    (resolve-manifest-ctrl manifest-first default-manifest-ctrl)
                    (resolve-manifest-ctrl {} default-manifest-ctrl))

        define-manifest-ctrl# ctrl-nrm#
        
        define-manifest-form# `(define-manifest-forms ~define-manifest-ctrl# ~@manifest-args)

        define-ctrl-defs# (for [[key# entry#] define-manifest-ctrl# :when (contains? entry# manifest-ctrl-key-def)]
                             (get entry# manifest-ctrl-key-def))]

    (doall (for [define-ctrl-def# define-ctrl-defs#]
             (do
               ;; have to eval the namer fns!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
               (eval define-ctrl-def#))))
    
    `(do
       ~define-manifest-form#)))

;; **************
;; FIN: manifests
;; **************

;; ***************
;; BEG: validators
;; ***************


(defn identify-extra-keys
  "identified the set of keys which are in test-keys
   but not all-keys. returns a map"
  [test-keys & all-keys]
  {:pre [(coll? test-keys)] :post [(map? %)]}
  (let [test-keys# (apply sorted-set (flatten test-keys))
        all-keys# (apply sorted-set (flatten all-keys))
        extra-keys# (set/difference test-keys# all-keys#)
        key-summary# (if (not-empty extra-keys#)
                       {:test-keys test-keys# :all-keys all-keys# :extra-keys extra-keys# }
                       {:test-keys test-keys# :all-keys all-keys#})]
    key-summary#))

(defn validate-keys-are-known
  "validates all test-keys are in known-keys else exception
   retrnus the summary from identify-extra-keys if ok"
  [test-keys & known-keys]
  {:pre [(coll? test-keys)] :post [(coll? %)]}
  (let [key-summary# (identify-extra-keys test-keys known-keys) ]
    (if (contains? key-summary# :extra-keys)
      (surprise-exception key-summary# "validate-keys-are-known" "found unexpected keys"))
    (get key-summary# :test-keys)))


(defn validate-map-keys-are-known
  [src-map & known-keys]
  {:pre [(map? src-map)] :post [(map? %)]}
  (let [src-keys (keys src-map)
        _ (if (and src-keys (not-empty src-keys)) (validate-keys-are-known  src-keys known-keys))]
    src-map))


;; ***************
;; FIN: validators
;; ***************

;; **********************
;; BEG: tracing manifests
;; **********************

(def manifest-linefeed \newline)

(def manifest-default :default)
(def manifest-format-name-default manifest-default)

(def manifest-formatters-default manifest-default)
(def manifest-enumerators-default manifest-default)
(def manifest-templates-default manifest-default)

(def manifest-formatter :formatter)
(def manifest-enumerator :enumerator)

;;(def trace-format-value-default-value manifest-default)

(def manifest-format-spec-key-mnemonic :mnemonic)
(def manifest-format-spec-key-value :value)
(def manifest-format-spec-key-filter :filter)
(def manifest-format-spec-key-merger :merger)
(def manifest-format-spec-key-formatter :formatter)
(def manifest-format-spec-key-enumerator :enumerator)

(def manifest-format-spec-keys
  [manifest-format-spec-key-mnemonic
   manifest-format-spec-key-filter
   manifest-format-spec-key-value
   manifest-format-spec-key-formatter
   manifest-format-spec-key-enumerator])

(def manifest-format-spec-fn-keys
  [manifest-format-spec-key-formatter
   manifest-format-spec-key-enumerator])

;; **********************
;; FIN: tracing manifests
;; **********************

;; ********************
;; BEG: tracing support
;; ********************


(defn merge-format-specifications-by-mnemonics
  [specifications & mnemonics]
  (reduce (fn [s m] merge s (get specifications m)) {} (flatten mnemonics)))

(defn make-fn-merge-format-specifications
  [formatters-collection enumerators-collection]
  {:pre [(map? formatters-collection) (map? enumerators-collection)] :post [(fn? %)]} 
  (fn [& {:keys [formatters enumerators]}]
    {:post [(map? %)]}
    (merge
     (merge-format-specifications-by-mnemonics formatters-collection formatters)
     (merge-format-specifications-by-mnemonics enumerators-collection  enumerators))))


(def trace-format-highlight-first-telltale-format-specification (atom "%s"))

(def trace-format-highlight-first-telltale
  (atom (fn [x] (format @trace-format-highlight-first-telltale-format-specification (str x)))))

(defn trace-format-highlight-first-arg
  [fn-highlighter & args]
  {:pre [(fn? fn-highlighter)]}
  (let [flattened-args (flatten args)
        highlighted-args (list* (fn-highlighter (first flattened-args)) (rest flattened-args))]
    highlighted-args))

(defn trace-format-sign-args
  [trace-sign & args]
  (let [flattened-args (flatten args)
        signed-args (list (trace-format-highlight-first-arg (deref trace-format-highlight-first-telltale) (first flattened-args)) trace-sign (rest flattened-args))]
    signed-args))

(defn trace-configure
  "configure values in atoms e.g highlighter format"
  [& {:keys [first-telltale-format-specification enable-diagnostics] :as config}]
  (let []
    (maybe-update-atom-one-arg trace-format-highlight-first-telltale-format-specification first-telltale-format-specification)
    (if enable-diagnostics (update-atom-one-arg carp-diagnostics-status enable-diagnostics)))
  config)

;; ********************
;; FIN: tracing support
;; ********************


;; *****************
;; BEG: tracing misc
;; *****************

;; controls whether tracing enabled or disabled
(def trace-state (atom nil))
(def trace-state-stack (atom []))

(defn get-trace [] @trace-state)

(defn trace-state-message
  [& telltales]
  (let [message (format-stringify-args "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
                                       "SET-STATE" (get-trace)
                                       telltales)]
    (doall (carp-diagnostic message))
    (doall (println message))))


(defn set-trace
  [arg-state & telltales]
  (assert (coll? telltales))
  (let [old-state (get-trace)
        new-state (if arg-state true false)]
    (if (not= new-state old-state)
      (swap! trace-state (fn [& args] new-state)))
    (trace-state-message "set-trace" "OLD-STATE" old-state telltales)))


(defn push-trace
  [state & telltales]
  (let [current-state (get-trace)]
    (swap! trace-state-stack (fn [current-stack] (conj current-stack current-state)))
    (set-trace state "PUSHED" current-state telltales)))

(defn pop-trace
  [& telltales]
  (let [old-state (get-trace)
        stack-state (peek @trace-state-stack)]
    (swap! trace-state-stack (fn [current-stack] (pop current-stack)))
    (if stack-state
      (set-trace stack-state "POPPED" "STACK-STATE" stack-state  telltales)
      (trace-state-message "NOT POPPED - STACK EMPTY" "RETAINED OLD-STATE" old-state telltales))
    stack-state))


(defmacro macro-set-trace
  [arg-state & telltales]
  (let [swap-form# `(set-trace ~arg-state ~@telltales)]
    (carp-diagnostic "MACRO-SET-TRACE" "SWAP-FORM" swap-form# "ARG-STATE" (class arg-state) arg-state telltales)
    `(do
       ~swap-form#))) 

(defmacro macro-push-trace
  [arg-state & telltales]
  (let [swap-form# `(push-trace ~arg-state ~@telltales)]
    (carp-diagnostic "MACRO-PUSH-TRACE" "SWAP-FORM" swap-form# "ARG-STATE" (class arg-state) arg-state telltales)
    `(do
       ~swap-form#))) 

(defmacro macro-pop-trace
  [& telltales]
  (let [swap-form# `(pop-trace ~@telltales)]
    (carp-diagnostic "MACRO-POP-TRACE" "SWAP-FORM" swap-form# telltales)
    `(do
       ~swap-form#)))

(defn trace-mark
  "main trace diagnostic"
  [& args]
  (if @trace-state
    (let [trace-message (format-stringify-args args)]
      (doall (println trace-message)))))


;; *****************
;; FIN: tracing misc
;; *****************


;; ***********************
;; BEG: tracing validators
;; ***********************

(defn validate-format-specification
  [format-spec]
  {:pre [(map? format-spec)] :post [(map? %)]}
  (validate-map-keys-are-known format-spec manifest-format-spec-keys)
  (for [fn-keyname manifest-format-spec-fn-keys]
    (let [fn-keyvalue (get format-spec fn-keyname)]
      (if fn-keyvalue (or (fn? fn-keyvalue) (surprise-exception {fn-keyname fn-keyvalue} "validate-format-specification" "KEY VALUE not fn")))))
  format-spec
  ) 

;; ***********************
;; FIN: tracing validators
;; ***********************


;; **********************
;; BEG: trace enumerators
;; **********************

;; enumerators are templateially of two types: for scalars and collections.
;; for each "loop" of the enumerator, the formatter is called on the
;; loop value

(declare trace-formatter-class-only)

(defn- trace-value-enumerator-scalar
  [enum-value fn-formatter & args]
  {:pre [(fn? fn-formatter)]}
  (list args (fn-formatter enum-value)))

;; just adding a \newlne to each line doesn't work - spurious space at
;; beginning of 2nd and subsequent lines
(defn- trace-value-enumerator-coll
  [enum-coll fn-formatter & args]
  {:pre [(coll? enum-coll) (fn? fn-formatter)]}
  (let [telltale (format-stringify-args args (trace-formatter-class-only enum-coll))]
    (if (not-empty enum-coll)
      (str/join manifest-linefeed (map-indexed (fn [ndx kv] (format-stringify-args telltale "ndx" ndx (fn-formatter kv)))  enum-coll))
      (format-stringify-args telltale "is empty"))))



;; **********************
;; FIN: trace enumerators
;; **********************


;; *********************
;; BEG: trace formatters
;; *********************

;; formatters take an item and produce a string for e.g. printing

(defn trace-formatter-full [x] (format ">%s< >%s<" (resolve-any-class-telltale x) x))
;;(defn trace-formatter-class [x] (format ">%s< >%s<" (resolve-any-class-telltale x) (str x)))
(defn trace-formatter-class-only [x] (format ">%s<" (resolve-any-class-telltale x)))
(defn trace-formatter-value-only [x] (format ">%s<" x))
(defn trace-formatter-map-kv [[k v]] (format "k >%s< v >%s< >%s<" k (resolve-any-class-telltale v) v))
(defn trace-formatter-map-kv-value-only [[k v]] (format "k >%s< v >%s<" k  v))
(defn trace-formatter-map-kv-class-only [[k v]] (format "k >%s< v >%s<" k  (resolve-any-class-telltale v)))


;; *********************
;; FIN: trace formatters
;; *********************


;; **************************************
;; BEG: tracing specification definitions
;; **************************************


(def manifest-enumerator-coll :coll)
(def manifest-enumerator-map :coll)
(def manifest-formatter-map :map-entry)

(def manifest-mnemonic-scalar :scalar)
(def manifest-mnemonic-coll :coll)
(def manifest-mnemonic-map :map)

(def manifest-mnemonic-map-class-only :map-class-only)
(def manifest-mnemonic-map-value-only :map-value-only)
(def manifest-mnemonic-vector :vector)
(def manifest-mnemonic-map-entry :map-entry)
(def manifest-mnemonic-map-entry-value-only :map-entry-no-class)

(def manifest-backstop-template-map manifest-mnemonic-map)
(def manifest-backstop-template-map-entry manifest-mnemonic-map-entry)
(def manifest-backstop-template-coll manifest-mnemonic-coll)

(def manifest-backstop-formatter-map manifest-mnemonic-map)
(def manifest-backstop-enumerator-coll manifest-mnemonic-coll)

(def trace-value-enumerators
  {manifest-mnemonic-coll {manifest-enumerator trace-value-enumerator-coll}
   manifest-mnemonic-map {manifest-enumerator trace-value-enumerator-coll}
   manifest-mnemonic-vector {manifest-enumerator trace-value-enumerator-coll}
   manifest-mnemonic-map-entry {manifest-enumerator trace-value-enumerator-scalar}
   manifest-mnemonic-scalar {manifest-enumerator trace-value-enumerator-scalar}
   manifest-enumerators-default {manifest-enumerator trace-value-enumerator-scalar}})



(def trace-value-formatters
  {
   ;; manifest-mnemonic-scalar {manifest-formatter trace-formatter-full}
   ;; manifest-mnemonic-coll {manifest-formatter trace-formatter-class}
   ;; manifest-mnemonic-vector {manifest-formatter trace-formatter-full}

   manifest-mnemonic-scalar {manifest-formatter trace-formatter-value-only}
   manifest-mnemonic-coll {manifest-formatter trace-formatter-value-only}
   manifest-mnemonic-vector {manifest-formatter trace-formatter-value-only}
   
   ;;manifest-mnemonic-map {manifest-formatter trace-formatter-map-kv}
   manifest-mnemonic-map {manifest-formatter trace-formatter-map-kv-value-only}

   manifest-mnemonic-map-value-only {manifest-formatter trace-formatter-map-kv-value-only}
   manifest-mnemonic-map-class-only {manifest-formatter trace-formatter-map-kv-class-only}

   ;;manifest-mnemonic-map-entry {manifest-formatter trace-formatter-map-kv}
   manifest-mnemonic-map-entry {manifest-formatter trace-formatter-map-kv-value-only}

   manifest-mnemonic-map-entry-value-only {manifest-formatter trace-formatter-map-kv-value-only}

   manifest-formatters-default {manifest-formatter trace-formatter-full}})


(def trace-value-formatters-default (get trace-value-formatters manifest-formatters-default))
(def trace-value-enumerators-default (get trace-value-enumerators manifest-enumerators-default))

(def merge-format-specifications (make-fn-merge-format-specifications trace-value-formatters trace-value-enumerators))

(def trace-value-templates-nom
  {manifest-templates-default {}
   
   manifest-mnemonic-scalar
   (merge-format-specifications
    :formatters manifest-mnemonic-scalar
    :enumerators manifest-mnemonic-scalar
    )

   manifest-mnemonic-map-entry
   (merge-format-specifications
    :formatters manifest-mnemonic-map-entry
    :enumerators manifest-mnemonic-map-entry
    )

   manifest-mnemonic-map-entry-value-only
   (merge-format-specifications
    :formatters manifest-mnemonic-map-entry-value-only
    :enumerators manifest-mnemonic-map-entry
    )
   
   manifest-mnemonic-map
   (merge-format-specifications
    :formatters manifest-mnemonic-map
    :enumerators manifest-mnemonic-map
    )

   manifest-mnemonic-map-value-only
   (merge-format-specifications
    :formatters manifest-mnemonic-map-value-only
    :enumerators manifest-mnemonic-map
    )

   manifest-mnemonic-map-class-only
   (merge-format-specifications
    :formatters manifest-mnemonic-map-class-only
    :enumerators manifest-mnemonic-map
    )

   manifest-mnemonic-coll
   (merge-format-specifications
    :formatters manifest-mnemonic-coll
    :enumerators manifest-mnemonic-coll
    )

   manifest-mnemonic-vector
   (merge-format-specifications
    :formatters manifest-mnemonic-vector
    :enumerators manifest-mnemonic-vector
    )})


(def trace-value-templates-aliases
  {
   ;;:class :kls
   ;;:map-no-class :map-no-kls
   })

(def trace-value-templates
  (reduce
   (fn [ s [k v]]
     (let [main-entry (or (get s k) (surprise-exception k "entry not found - can not be aliased"))
           aliases (to-vector v)
           alias-map (into {}  (for [alias aliases] [alias main-entry]))]
       (merge s alias-map)))
   trace-value-templates-nom
   trace-value-templates-aliases))


#_(if @carp-diagnostics-status
  (doall (for [[k v] trace-value-templates]
           (do
             (carp-diagnostic "TRACE-VALUE-TEMPLATES" "k" (class k) k "v" (class v) v)
             (validate-format-specification v)))))


(def trace-value-templates-default (get trace-value-templates manifest-templates-default))


;; **************************************
;; FIN: tracing specification definitions
;; **************************************

;; ************************************
;; BEG: format specifications accessors
;; ************************************


(defn define-fn-format-specification-get-key
  [keyname telltale]
  {:pre [(keyword? keyname)] :post [(fn? %)]}
  (fn [fmt-spec]
    {:pre [(map? fmt-spec)]}
    (let [keyvalue  (get fmt-spec keyname)]
      keyvalue)))


(defn define-fn-format-specification-put-key
  [keyname]
  {:pre [(keyword? keyname)] :post [(fn? %)]}
  (fn [fmt-spec keyvalue]
    {:pre [(map? fmt-spec)] :post [(map? %)]}
    (assoc fmt-spec keyname keyvalue)))


(def format-specification-get-mnemonic (define-fn-format-specification-get-key manifest-format-spec-key-mnemonic "MNEMONIC"))
(def format-specification-get-value (define-fn-format-specification-get-key manifest-format-spec-key-value "VALUE"))
(def format-specification-get-filter (define-fn-format-specification-get-key manifest-format-spec-key-filter "FILTER"))
(def format-specification-get-formatter (define-fn-format-specification-get-key manifest-format-spec-key-formatter "FORMATTER"))
(def format-specification-get-enumerator (define-fn-format-specification-get-key manifest-format-spec-key-enumerator "ENUMERATOR"))

(def format-specification-put-mnemonic (define-fn-format-specification-put-key manifest-format-spec-key-mnemonic))
(def format-specification-put-value (define-fn-format-specification-put-key manifest-format-spec-key-value))
(def format-specification-put-filter (define-fn-format-specification-put-key manifest-format-spec-key-filter))
(def format-specification-put-formatter (define-fn-format-specification-put-key manifest-format-spec-key-formatter))
(def format-specification-put-enumerator (define-fn-format-specification-put-key manifest-format-spec-key-enumerator))


;; ************************************
;; FIN: format specifications accessors
;; ************************************


;; *********************************
;; BEG: format specification finders
;; *********************************

(defn make-fn-get-specification
  ([specifications telltale] (make-fn-get-specification specifications telltale nil))
  ([specifications telltale default]
     {:pre [(map? specifications)] :post [(fn? %)]}
     (fn [mnemonic]
       {:post [(or (nil? %) (map? %))]}
       (let [value (get specifications mnemonic default)]
         value))))


(def get-specification-enumerator (make-fn-get-specification trace-value-enumerators "ENUMERATOR"))
(def get-specification-formatter (make-fn-get-specification trace-value-formatters "FORMATTER"))
(def get-specification-template (make-fn-get-specification trace-value-templates "TEMPLATE"))

(def find-specification-enumerator (make-fn-get-specification trace-value-enumerators "ENUMERATOR" manifest-enumerators-default))
(def find-specification-formatter (make-fn-get-specification trace-value-formatters "FORMATTER" manifest-formatters-default))
(def find-specification-template (make-fn-get-specification trace-value-templates "TEMPLATE" manifest-templates-default))


;; *********************************
;; BEG: format specification finders
;; *********************************


;; **********************************
;; BEG: specification resolver makers
;; **********************************


(defn make-fn-find-format-specification-wrapper
  "makes a wrapper function that takes a format-spec map,
   gets a value from it using the fn-find-arg fn
   and calls the fn-find-spec fn with the found value to get the result spec"
  [fn-find-arg fn-find-spec telltale]
  {:pre [(fn? fn-find-arg) (fn? fn-find-spec)] :post [(fn? %)]}
  (fn  [format-spec]
    {:pre [(map? format-spec)] :post [(or (nil? %) (map? %))]}
    (let [find-arg (fn-find-arg format-spec)
          find-spec (fn-find-spec find-arg)]
      find-spec)))


(defn make-fn-resolve-format-specification
  "makes a fn to call a find specification fn
   with the passed format-spec as argument
   IF predicate function returns NIL.
   the predicate can be used to e.g. check if a formatter has already been found.
   merges the found spec with the passed spec and retruns the merged map"
  [fn-find-spec fn-find-pred telltale]
  {:pre [(fn? fn-find-spec) (fn? fn-find-pred)] :post [(fn? %)]}
  (fn  [format-spec]
    {:pre [(map? format-spec)] :post [(map? %)]}
    (let [find-spec (if-not (fn-find-pred format-spec) (fn-find-spec format-spec))
          updated-format-spec
          (if-not find-spec
            format-spec
            (if (contains? find-spec manifest-format-spec-key-merger)
              ((get find-spec manifest-format-spec-key-merger) format-spec find-spec)
              (merge format-spec find-spec)) )]
      updated-format-spec)))


(defn predicate-always-find-format-specification [& args]
  (carp-diagnostic "predicate-always-find-format-specification" "ARGS" args)
  nil)


;; **********************************
;; FIN: specification resolver makers
;; **********************************


;; ***********************************
;; BEG: find specification by mnemonic
;; ***********************************


(defn make-fn-find-format-specification-by-mnemonic
  [fn-find-spec telltale]
  {:pre [(fn? fn-find-spec)] :post [(fn? %)]}
  (fn [mnemonic]
    {:pre [(or (nil? mnemonic) (keyword? mnemonic))] :post [(or (nil? %) (map? %))]}
    (let [find-spec  (fn-find-spec mnemonic)]
      (carp-diagnostic telltale "CALL" "MNEMONIC" (class mnemonic) mnemonic)
      (carp-diagnostic telltale "CALL" "FIND-SPEC" (class find-spec) find-spec)
      find-spec)))


(def find-formatter-by-mnemonic (make-fn-find-format-specification-by-mnemonic get-specification-formatter "find-formatter-by-mnemoni"))
(def find-enumerator-by-mnemonic (make-fn-find-format-specification-by-mnemonic get-specification-enumerator "find-enumerator-by-mnemonic"))

(def find-template-by-mnemonic-nom (make-fn-find-format-specification-by-mnemonic get-specification-template "find-template-by-mnemonic-nom"))

;; need to add a default clause
(defn find-template-by-mnemonic
  [mnemonic]
  {:pre [(or (nil? mnemonic) (keyword? mnemonic))] :post [(or (nil? %) (map? %))]}
  (let [find-spec-nom (if mnemonic (find-template-by-mnemonic-nom mnemonic))
        find-spec (or find-spec-nom trace-value-templates-default)]
    find-spec))


;; ***********************************
;; FIN: find specification by mnemonic
;; ***********************************


;; *****************************************
;; BEG: find backstop specification by value
;; *****************************************


(defn find-backstop-formatter-by-value
  [value]
  {:post [(map? %)]}
  (cond
   (map? value) (find-specification-formatter manifest-backstop-formatter-map)
   :else trace-value-formatters-default))

(defn find-backstop-enumerator-by-value
  [value]
  {:post [(map? %)]}
  (cond
   (coll? value) (find-specification-enumerator manifest-backstop-enumerator-coll)
   :else trace-value-enumerators-default))

(defn find-backstop-template-by-value
  [value]
  {:post [(map? %)]}
  (cond
   (map? value) (find-specification-template manifest-backstop-template-map)
   (instance? clojure.lang.MapEntry value) (find-specification-template manifest-backstop-template-map-entry)
   (coll? value) (find-specification-template manifest-backstop-template-coll)
   :else trace-value-templates-default))


;; *****************************************
;; FIN: find backstop specification by value
;; *****************************************


;; ************************
;; BEG: find  specification 
;; ************************


(def find-format-specification-backstop-formatter (make-fn-find-format-specification-wrapper
                                                   format-specification-get-value
                                                   find-backstop-formatter-by-value
                                                   "BACKSTOP-FORMATTER"))

(def find-format-specification-backstop-enumerator (make-fn-find-format-specification-wrapper
                                                    format-specification-get-value
                                                    find-backstop-enumerator-by-value
                                                    "BACKSTOP-ENUMERATOR"))

(def find-format-specification-backstop-template (make-fn-find-format-specification-wrapper
                                                  format-specification-get-value
                                                  find-backstop-template-by-value
                                                  "BACKSTOP-TEMPLATE"))

(def find-format-specification-mnemonic-formatter (make-fn-find-format-specification-wrapper
                                                   format-specification-get-mnemonic
                                                   find-formatter-by-mnemonic
                                                   "MNEMONIC-FORMATTER"))

(def find-format-specification-mnemonic-enumerator (make-fn-find-format-specification-wrapper
                                                    format-specification-get-mnemonic
                                                    find-enumerator-by-mnemonic
                                                    "MNEMONIC-ENUMERATOR"))

(def find-format-specification-mnemonic-template (make-fn-find-format-specification-wrapper
                                                  format-specification-get-mnemonic
                                                  find-template-by-mnemonic
                                                  "MNEMONIC-TEMPLATE"))


;; ************************
;; FIN: find  specification 
;; ************************



;; ****************************
;; BEG: specification resolvers
;; ****************************


(def resolve-format-specification-backstop-formatter (make-fn-resolve-format-specification
                                                      find-format-specification-backstop-formatter
                                                      ;;get-specification-formatter
                                                      format-specification-get-formatter
                                                      "BACKSTOP-FORMATTER"))

(def resolve-format-specification-backstop-enumerator (make-fn-resolve-format-specification
                                                       find-format-specification-backstop-enumerator
                                                       ;;get-specification-enumerator
                                                       format-specification-get-enumerator
                                                       "BACKSTOP-ENUMERATOR"))

(def resolve-format-specification-backstop-template (make-fn-resolve-format-specification
                                                     find-format-specification-backstop-template
                                                     get-specification-template
                                                     ;;format-specification-get-template
                                                     "BACKSTOP-TEMPLATE"))

(def resolve-format-specification-formatter (make-fn-resolve-format-specification
                                             find-format-specification-mnemonic-formatter
                                             format-specification-get-formatter
                                             "FORMATTER"))

(def resolve-format-specification-enumerator (make-fn-resolve-format-specification
                                              find-format-specification-mnemonic-enumerator
                                              format-specification-get-enumerator
                                              "ENUMERATOR"))

(def resolve-format-specification-template (make-fn-resolve-format-specification
                                            find-format-specification-mnemonic-template
                                            predicate-always-find-format-specification
                                            "TEMPLATE"))


;; ****************************
;; FIN: specification resolvers
;; ****************************


;; ************
;; BEG: filters
;; ************

(def filter-directives-dictionary
  {:select-keys 1
   :filter-keys 1
   :telltale 1})

(defn apply-filters-resolve-directives
  [filter-directives]
  {:pre [(vector? filter-directives)] :post [(map? %)]}
  (let [resolved-directives
        (apply array-map
               (loop [kv-pairs [] directives filter-directives]
                 (if (and (coll? directives) (not-empty directives))
                   (let [directive (first directives)
                         ;; how many args for this directive
                         directive-args-count (get filter-directives-dictionary directive 0)

                         directive-args (cond
                                         (= 0 directive-args-count) nil
                                         (= 1 directive-args-count) (first (rest directives))
                                         :else (take directive-args-count (rest directives)))]
                     
                     (recur (concat kv-pairs [directive directive-args]) (drop (+ 1 directive-args-count) directives)))
                   kv-pairs)))]
    
    resolved-directives))


(defn apply-filters
  [format-spec]
  {:pre [(map? format-spec)] :post [(map? %)]}
  (let [filter-spec-nom (format-specification-get-filter format-spec)
        updated-format-spec (if-not filter-spec-nom
                              format-spec
                              (let [filter-directives (apply-filters-resolve-directives (to-vector filter-spec-nom))
                                    filter-value (format-specification-get-value format-spec)
                                    
                                    updated-filter-value
                                    (reduce (fn [s [fn-name fn-args]]
                                              (if fn-args
                                                (apply fn-name fn-args)
                                                (fn-name s)))
                                            filter-value
                                            filter-directives)]
                                (format-specification-put-value format-spec updated-filter-value)))]
    updated-format-spec))


;; ************
;; FIN: filters
;; ************


(defn trace-value-resolve-specification
  [format-spec]
  {:pre [(map? format-spec)] :post [(map? %)]}
  (-> format-spec
      validate-format-specification
      apply-filters
      resolve-format-specification-template
      resolve-format-specification-formatter
      resolve-format-specification-backstop-formatter
      resolve-format-specification-enumerator
      resolve-format-specification-backstop-enumerator))

(defn normalise-format-specification
  [format-spec-nom]
  {:post [(or (nil? %) (map? %))]}
  (let [format-spec (cond
                     (map? format-spec-nom) format-spec-nom
                     (keyword? format-spec-nom) {manifest-format-spec-key-mnemonic format-spec-nom}
                     (string? format-spec-nom) nil
                     :else (surprise-exception format-spec-nom "format-spec is what?")
                     )]
    (if format-spec (validate-format-specification format-spec))))


;; ****************
;; FIN: format spec
;; ****************

;; *****************
;; BEG: trace values
;; *****************


(defn trace-value-format
  "main fn to format a value
  the format-name args is interpreted as the format to use
  but if not found, its assumed part of args i.e. telltales
  the value is used to find the enumerator e.g. for a collection or scalar (default)
  returns a list of elements to be e.g. printed"
  ([value] (trace-value-format value manifest-format-name-default))
  ([value  & args]
     (let [flatten-args (flatten args)
           format-spec-maybe (normalise-format-specification (first flatten-args))

           format-spec-norm (or format-spec-maybe {})
           
           ;; if spec found, remove from args to prevent it printing
           normalised-args (if format-spec-maybe
                             (trace-format-highlight-first-arg @trace-format-highlight-first-telltale (rest flatten-args))
                             (trace-format-highlight-first-arg @trace-format-highlight-first-telltale flatten-args))

           format-spec (trace-value-resolve-specification (format-specification-put-value format-spec-norm value))
           
           fn-formatter (format-specification-get-formatter format-spec)
           fn-enumerator (format-specification-get-enumerator format-spec)
           normalised-value (format-specification-get-value format-spec)]

       (fn-enumerator normalised-value  fn-formatter normalised-args))))



(defn trace-value
  ([value] (trace-value value manifest-format-name-default))
  ([value & args]
     (if @trace-state (trace-mark (trace-value-format value args)))
     ;; always return the passed value for fluent interface
     value))

;; *****************
;; FIN: trace values
;; *****************


;; **************************
;; BEG: packaged trace points
;; **************************

(def trace-entr-sign ">>>>ENTR")
(def trace-exit-sign "EXIT<<<<")
(def trace-call-sign "<<CALL>>")
(def trace-body-sign "  BODY  ")

(defn make-fn-trace-signed
  [trace-sign]
  (fn [& args]
    (if @trace-state
      (trace-mark (trace-format-sign-args trace-sign args)))))

(def trace-entr (make-fn-trace-signed trace-entr-sign))
(def trace-exit (make-fn-trace-signed trace-exit-sign))
(def trace-call (make-fn-trace-signed trace-call-sign))
(def trace-body (make-fn-trace-signed trace-body-sign))

(defn make-fn-trace-value-signed
  [trace-sign]
  (fn  [value mnemonic & args]
    (if @trace-state
      (cond
       (map? mnemonic) (trace-value value mnemonic (trace-format-sign-args trace-sign args))
       (and (keyword? mnemonic) (get-specification-template mnemonic)) (trace-value value mnemonic (trace-format-sign-args trace-sign args))
       :else (trace-value value (trace-format-sign-args trace-sign mnemonic args))))))

(def trace-value-entr (make-fn-trace-value-signed trace-entr-sign))
(def trace-value-exit (make-fn-trace-value-signed trace-exit-sign))
(def trace-value-call (make-fn-trace-value-signed trace-call-sign))
(def trace-value-body (make-fn-trace-value-signed trace-body-sign))


;; **************************
;; FIN: packaged trace points
;; **************************

