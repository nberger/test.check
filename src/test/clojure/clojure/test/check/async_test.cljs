(ns clojure.test.check.async-test
  (:require [cljs.test :as test :refer-macros [deftest testing is async]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen :include-macros true]
            [clojure.test.check.properties :as prop :include-macros true]
            [clojure.test.check.results :as results]
            [clojure.test.check.rose-tree :as rose]
            [clojure.test.check.clojure-test :as ct]))

(deftest basic-successful-async-test
  (async done
    (testing "a successful async quick-check"
      (tc/async-quick-check
        100
        (prop/for-all* [gen/s-pos-int]
          (fn [x]
            (fn [trial-done-fn]
              (js/setTimeout #(trial-done-fn (pos? x)) 10))))
        :step-fn (comp
                   (fn [{:keys [step] :as qc-state}]
                     (if (= :succeeded step)
                       (do
                         (println "finished successful async qc!")
                         (done))
                       qc-state))
                   ct/default-step-fn)))))

(deftest basic-failing-async-test
  (async done
    (testing "a failing async quick-check"
      (tc/async-quick-check
        100
        (prop/for-all* [(gen/vector gen/s-pos-int)]
          (fn [coll]
            (fn [trial-done-fn]
              (js/setTimeout #(trial-done-fn (every? (partial > 10) coll)) 10))))
        :step-fn (fn [{:keys [step] :as qc-state}]
                   (case step
                     :failed
                     (do
                       (println "failed async qc!" (-> (dissoc qc-state :property)
                                                       (assoc :args (-> qc-state :result-map-rose rose/root :args))))
                       qc-state)
                     :shrunk
                     (do
                       (println "finished failing async qc!" (dissoc qc-state :property))
                       (done))
                     qc-state))))))

(deftest normal-async-test
  (async done
    (js/setTimeout
      (fn []
        (println "normal async test")
        (is true)
        (done))
      10)))
