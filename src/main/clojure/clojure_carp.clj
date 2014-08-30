(ns clojure-carp
  "Diagnostic, Exception and Validation functions for Clojure"
  (:require [clojure.walk :as walkies])
  (:import (java.lang Exception Class)))

;; *************************
;; BEG: internal diagnostics
;; *************************

(def ^:private highlight-format-carp-diagnostic "%-30s" )

(def ^:private carp-diagnostics-status (atom false))

(defn carp-diagnostic-set [value] (reset! carp-diagnostics-status value))

(defn format-highlight-carp-diagnostic [value] (format highlight-format-carp-diagnostic value))

(defn carp-diagnostic
  [telltale & args]
  (if @carp-diagnostics-status
    (dorun (apply println (format-highlight-carp-diagnostic telltale) args))))

;; *************************
;; FIN: internal diagnostics
;; *************************

;; *************************************
;; BEG: misc exception support functions
;; *************************************

(defn- format-stringify-args
  [& args]
  (let [normalised-args (flatten args)]
    (apply str (interpose " " normalised-args))))

(defn- format-exception-value [value] (str "Value " (format ">%s< >%s<" (class value) value)))
(defn- format-assertion-value [value] (str "Value " (format ">%s< >%s<" (class value) value)))

(defn- is-call-form?
  [form]
  (let [call-form? (and (sequential? form) (not (vector? form)))]
    call-form?))

;; *************************************
;; FIN: misc exception support functions
;; *************************************

;; *************************
;; BEG: collection functions
;; *************************

(defn- to-collection
  [any]
  (cond
   (coll? any) any
   (nil? any) (list)
   :else (list any)))

(defn- to-set
  [any]
  (into #{} (remove nil? (to-collection any))))

;; *************************
;; BEG: collection functions
;; *************************

;; ***************
;; BEG: exceptions
;; ***************

(defn raise-exception
  "throw an exception"
  [& args]
  (let [exception-message (format-stringify-args args)]
    (doall (println "RAISE EXCEPTION" exception-message))
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
  [& args]
  (raise-exception "Trace Exception"  args))

(defn logic-exception
  [value & args]
  (raise-exception "Logic Exception" (format-exception-value value) args))

(defn contract-exception
  [value & args]
  (raise-exception "Contract Exception" (format-exception-value value) args))

(defn condition-exception
  [value & args]
  (raise-exception "Condition Exception" (format-exception-value value) args))

(defn predicate-exception
  [value & args]
  (raise-exception "Predicate Exception" (format-exception-value value) args))

;; ***************
;; FIN: exceptions
;; ***************

;; **************
;; BEG: reporting
;; **************

(defn- report-assertion-failure
  "throw an assertion-failure"
  [& args]
  (let [assertion-failure-message (format-stringify-args args)]
    (doall (println "ASSERTION-FAILURE" assertion-failure-message))))

(defn- report-contract-failure
  [value & args]
  (report-assertion-failure "Contract Failure" (format-assertion-value value) args))

;; **************
;; FIN: reporting
;; **************

;; *********************
;; BEG: unless exception
;; *********************

(defn- form-telltale-with-marker
  [any marker]
  {:post [(string? %)]}
  (cond
   (coll? any)
   (let [telltale-any# (apply str  (interpose " " any))]
     (apply str marker "(" telltale-any# ")"))
   :else (str marker "(" any ")")))

(defn- form-telltale-condition [pred] (form-telltale-with-marker pred "condition"))
(defn- form-telltale-predicate [pred] (form-telltale-with-marker pred "predicate"))

(defmacro unless-exception
  "cuases an exception if the unless-conditions are not true"
  [unless-condition & unless-consequence]
  (let [fn-telltale# "unless-exception"

        first-unless-consequence# (first unless-consequence)
        count-unless-consequence# (count unless-consequence)

        unless-consequence# (cond

                             ;; is this a call - leave alone
                             (and (= 1 count-unless-consequence#)
                                  (is-call-form? first-unless-consequence#))
                             first-unless-consequence#

                             :else
                             (let [telltale-unless-condition# (form-telltale-condition unless-condition)
                                   telltale-unless-exception# "UNLESS EXCEPTION"]

                               `(condition-exception ~telltale-unless-condition# ~telltale-unless-exception#  ~@unless-consequence)))

        form-unless# `(~'if-not ~unless-condition ~unless-consequence#)]

    `(do
       ~form-unless#)))

;; *********************
;; FIN: unless exception
;; *********************

;; *******************
;; BEG: validation fns
;; *******************

;; Defines an is-x? function using a predicate retruns the value of false
;; Defines a mustbe-x function that applies the predicate and return the value or raises a predicate exception

(defn- define-validation-fn-name-is [name-stem] (symbol (str "is-" name-stem "?")))
(defn- define-validation-fn-name-is-many [name-stem] (symbol (str "is-many-" name-stem "?")))
(defn- define-validation-fn-name-mustbe [name-stem] (symbol (str "mustbe-" name-stem)))
(defn- define-validation-fn-name-mustbe-many [name-stem] (symbol (str "mustbe-many-" name-stem)))

(def form-validation-functions-template
  {

   :mustbe
   {:namer define-validation-fn-name-mustbe
    :form '(defn walked-fn-name [x & more-telltales] (if (walked-fn-pred x) x (walked-fn-exception x walked-telltales more-telltales)))}

   :mustbe-many
   {:namer define-validation-fn-name-mustbe-many
    :form '(defn walked-fn-name [coll-x & more-telltales] {:pre [(sequential? coll-x)]} (if (every? walked-fn-pred coll-x) coll-x (walked-fn-exception (seq coll-x) walked-telltales more-telltales)))}

   :is
   {:namer define-validation-fn-name-is
    :form '(defn walked-fn-name [x] (if (walked-fn-pred x) x false))}

   :is-many
   {:namer define-validation-fn-name-is-many
    :form '(defn walked-fn-name [coll-x] {:pre [(sequential? coll-x)]} (if (every? walked-fn-pred coll-x) coll-x false))
    }

   })

(def form-validation-functions-known-fns  (into #{} (keys form-validation-functions-template)))

(defn form-validation-function
  [fn-template form-template]
  {:pre [(map? fn-template) (map? form-template) (every? keyword? (keys form-template))]}
  (let [fn-telltale# "form-validation-function"

        fn-name# (cond
                  ;; explicit name?
                  (contains? fn-template :name) (get fn-template :name)
                  ;; make the name?
                  (and  (contains? fn-template :namer) (contains? form-template :fn-name-stem)) ((get fn-template :namer) (get form-template :fn-name-stem))
                  :else (surprise-exception fn-template fn-telltale#  "no idea how to name fn"))

        fn-pred# (get form-template :fn-pred)
        fn-form# (get fn-template :form)

        walk-map-keywords#  (merge
                             (select-keys form-template [:telltales :fn-pred :fn-exception])
                             {:fn-name fn-name#})

        walk-map-symbols#  (into {} (map
                                     (fn [[k v]] [(symbol (str "walked-" (name k))) v])
                                     walk-map-keywords#))

        walked-fn-form# (walkies/prewalk-replace walk-map-symbols# fn-form#)]

    walked-fn-form#))

(defn form-validation-functions
  [ & {:syms [fns also-fns name-stem names predicate telltales exception]
       :as opt-args :or {fns [:mustbe :is]
                         exception `predicate-exception}}]
  (let [fn-telltale# "form-validation-functions"

        ;; all known args
        _ (assert (every? #{'fns 'also-fns 'name-stem 'names 'predicate 'telltales} (keys opt-args)))

        _ (if name-stem  (assert (symbol? name-stem)))

        forms-validation# (list) ;; TESTING

        ;; what validation fns are wanted?
        fn-val-set# (to-set (apply concat (map to-collection (list fns also-fns))))

        ;; all known fns??
        _ (assert (every? form-validation-functions-known-fns  fn-val-set#))

        ;; ensure any explicit names for known functions
        fn-names# (if names
                    (do
                      (assert (and (map? names)
                                   (every? form-validation-functions-known-fns (keys names))
                                   (every? symbol? (vals names))))
                      names)
                    {})

        ;; is fn - the returns value (c.f. true) or false

        form-template#  {:telltales telltales
                         :fn-pred predicate
                         :fn-name-stem name-stem
                         :fn-exception exception}

        ;; build the forms
        forms-map# (reduce
                    (fn [s fn-val]
                      (let [fn-val-template# (get form-validation-functions-template fn-val)

                            ;; an explcit name for this fn given?
                            fn-val-template# (if (contains? fn-names# fn-val)
                                               (assoc fn-val-template# :name (get fn-names# fn-val))
                                               fn-val-template#)

                            fn-val-form# (form-validation-function fn-val-template# form-template#)]
                        (assoc s fn-val fn-val-form#)))
                    {}
                    fn-val-set#)

        forms-validation# (vals forms-map#)]

    forms-validation#))

(defmacro define-canonical-validation-functions
  "see form-validation-functions"
  ([ & {:as opt-args}]
     (let [fn-telltale# "define-canonical-validation-functions"
           forms-validation-functions# (apply form-validation-functions (apply concat opt-args))]
       `(do
          ~@(for [c# forms-validation-functions#] c#)))))

(defmacro define-validation-functions
  "see form-validation-functions"
  ([fn-name-stem fn-pred  & {:as opt-args}]
     (let [fn-telltale# "define-validation-functions"

           all-args# (assoc opt-args
                       'name-stem fn-name-stem
                       'predicate fn-pred
                       )
           form-validation-functions# (list* `define-canonical-validation-functions (apply concat all-args#))]
       `(do
          ~form-validation-functions#))))

;; *******************
;; BEG: validation fns
;; *******************

