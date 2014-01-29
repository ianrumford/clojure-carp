(ns carp-tests1
  (:use [clojure.test :only [deftest is function?]]
        ;;clojure.core.contracts.impl.transformers
        ;;clojure.core.contracts
        )
  (:require [clojure-carp
             :as carp
             :refer (surprise-exception trace-exception contract-exception
                                        trace-mark trace-entr trace-exit trace-call trace-body
                                        trace-value trace-value-entr trace-value-exit trace-value-call trace-value-body)]))



(def base-fn1
  (fn [x]
    (println "THIS IS FN1: x" (class x) x)
    x
    ))

(carp/macro-set-diagnostics false)


(def fn-test3 base-fn1)

(def trace-telltale-main "trace-telltale-main")

(def test-suite-carp-mark1
  [

   [trace-mark "this" "is" "trace" "makr"]
   [trace-entr  trace-telltale-main "ENTR" "this" "is" "trace" "entr"]
   [trace-exit  trace-telltale-main "EXIT" "this" "is" "trace" "exit"]
   [trace-call  trace-telltale-main "CALL" "this" "is" "trace" "call"]
   
   ])


(deftest test-carp-mark1
  (doall (for [[fn-trace & args] test-suite-carp-mark1]
           (let []
             (doall (println "test-carp-mark1" "FN-TRACE" fn-trace "ARGS" args))
             (fn-trace args)
             (is (nil? (fn-trace args)))))))

(declare manifest-man1)

(defn fn-source-name
  [x]
  (str "source-namer-" (carp/resolve-name-from-any x)))

;;(defn fn-source-symbol [x] (symbol (fn-source-name x)))


(defn fn-target-name
  [x]
  (str "target-namer-"  (carp/resolve-name-from-any x)))


(def fn-src-namer1 '(fn [x] (symbol (str "src-namer1-" (clojure-carp/resolve-name-from-any x)))))
(def fn-tgt-namer1 '(fn [x] (str "tgt-namer1-" (clojure-carp/resolve-name-from-any x))))


(def fn-src-namer2 '(fn [x] (str "src-namer2-" (clojure-carp/resolve-name-from-any x))))
(def fn-tgt-namer2 '(fn [x] (str "tgt-namer2-" (clojure-carp/resolve-name-from-any x))))


(def test-suite-manifest1
  [

   [:man1 'manifest-man1]
   [[:man2 'my-man2] 'my-man2]

   [[:pivot 'manifest-pivot-field] 'manifest-pivot-field]
   
   [[:man3 :my-man3] 'manifest-my-man3]

   [['manifest-man1 :my-man4] 'manifest-my-man4] ;; need manifest-man1 defined for this to work

   [['manifest-man1 'my-man5] 'my-man5] ;; need manifest-man1 defined for this to work
   

   [:man6 'tgt-namer1-man6 {:target-namer fn-tgt-namer1}]

   ;; explicitly specified names - note are literals
   [:man8 'man8 {:target-namer '(fn [v] v)  :source-namer '(fn [v] v)}]

   [[:man7 'src-namer1-man7] 'src-namer1-man7]
   [:man7 'tgt-namer1-man7 {:target-namer fn-tgt-namer1 :source-namer fn-src-namer1}] 

   [:man9 'tgt-namer2-man9 {:target-namer fn-tgt-namer2}]
   
   ])

(def test-suite-manifest1-a
  [

   [:man1 'manifest-man1]

   ;;[[:man2 'my-man2] 'my-man2]

   ;;[:man5 'target-namer-man5 {:target-namer fn-target-name}]

   ;;[['manifest-man1 :my-man4] 'manifest-my-man4] ;; need manifest-man1 defined for this to work

   ;;   [['manifest-man1 'my-man5] 'my-man5]

   ;;[[:man7 'src-namer1-man7] 'src-namer1-man7]
   ;;[:man7 'target-namer-man7 {:target-namer fn-target-name :source-namer fn-source-symbol}] wont work - uses run-time fns
   ;;[:man7 'tgt-namer1-man7 {:target-namer fn-tgt-namer1 :source-namer fn-src-namer1}] 
   
   ;;[:man7 'man7 {:target-namer '(fn [v] v)  :source-namer '(fn [v] v)}]

   ;;[:man9 'tgt-namer2-man9 {:target-namer fn-tgt-namer2}]


   
   ])

(deftest test-manifest1
  (doall (for [[man-def test-symbol man-ctrl] test-suite-manifest1]
           (do
             (doall (println "test-manifest1" "ENTR" "MAN-DEF" (class man-def) man-def))
             (doall (println "test-manifest1" "ENTR" "TEST-SYMBOL" (class test-symbol) test-symbol))
             (doall (println "test-manifest1" "ENTR" "MAN-CTRL" (class man-ctrl) man-ctrl))
             (let [
                   man-args (if man-ctrl (list man-ctrl man-def) (list  man-def))
                   
                   return-var (eval `(carp/define-manifests  ~@man-args))
                   ;;return-var (eval `(carp/define-manitest  ~@man-args))

                   _ (doall (println "test-manifest1" "BODY" "RETURN-VAR" (class return-var) return-var))
                   
                   wanted-value (cond
                                 (keyword? man-def) man-def
                                 (vector? man-def)
                                 (let [first-def (first man-def)]
                                   (cond
                                    (keyword? first-def) first-def
                                    (symbol? first-def) (var-get (resolve first-def)))))


                   _ (doall (println "test-manifest1" "BODY" "WANTED-VALUE" (class wanted-value) wanted-value))

                   return-value (var-get return-var)
                   _ (doall (println "test-manifest1" "BODY" "RETURN-VALUE" (class return-value) return-value))
                   
                   ;; ;; test-value nil
                   ;; ;; test-value (resolve test-symbol)
                   test-value (var-get (resolve test-symbol))
                   
                   _ (doall (println "test-manifest1" "BODY" "TEST-VALUE" (class test-value) test-value))
                   
                   ]
               
               (doall (println "test-manifest1" "EXIT" "MAN-DEF" (class man-def) man-def))
               (doall (println "test-manifest1" "EXIT" "WANTED-VALUE" (class wanted-value) wanted-value))
               (doall (println "test-manifest1" "EXIT" "TEST-SYMBOL" (class test-symbol) test-symbol))
               (doall (println "test-manifest1" "EXIT" "TEST-VALUE" (class test-value) test-value))

               (doall (println "test-manifest1" "EXIT" "RETURN-VAR" (class return-var) return-var))
               (doall (println "test-manifest1" "EXIT" "RETURN-VALUE" (class return-value) return-value))

               (is (= return-value wanted-value)) ;; works
               (is (= test-value wanted-value)) ;; works
               
     
               )))))












;; PARK


                   ;;return-value nil
                   ;;return-value (var test-symbol)
                   ;;return-value (deref  test-symbol)
                   ;;return-value (var-get (var return-var))
;; (def base-fn2 (fn [& x]
;;            (println "THIS IS FN2: x" (class x) x)
;;            x
;;            ))

;; (def fn1-apply base-fn1)
;; (def fn1-update base-fn1)

;; (def random-string1 "this-is-random-string")


;; (def manifest-key1 :key1)
;; (def manifest-key2 :key2)
;; (def manifest-key3 :key3)


;; (def base-map1 {:a 1 :b 2 :c 3})
;; (def base-vec1 [:x :y :z])


;; (def base-fn0 (fn [& x]
;;            (println "THIS IS FN0: x" (class x) x)
;;            x
;;            ))
