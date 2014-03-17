(ns clojure-carp
  (:import (java.lang Exception Class)))

;; *************************************
;; BEG: misc exception support functions
;; *************************************

(defn format-stringify-args
  [& args]
  (let [normalised-args (flatten args)]
    #_(doall (for [arg normalised-args] (doall (println (format "format-stringify-args ARG ^%s^ ^%s^" (class arg) arg)))))
    (apply str (interpose " " normalised-args))))

(defn format-exception-value [value] (str "Value " (format ">%s< >%s<" (class value) value)))
(defn format-assertion-value [value] (str "Value " (format ">%s< >%s<" (class value) value)))

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

;; **************
;; BEG: reporting
;; **************

(defn report-assertion-failure
  "throw an assertion-failure"
  [& args]
  (let [assertion-failure-message (format-stringify-args args)]
    (doall (println "ASSERTION-FAILURE" assertion-failure-message))))

(defn report-contract-failure
  [value & args]
  (report-assertion-failure "Contract Failure" (format-assertion-value value) args))

;; **************
;; FIN: reporting
;; **************
