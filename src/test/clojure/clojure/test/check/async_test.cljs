(ns clojure.test.check.async-test
  (:require [cljs.test :as test :refer-macros [deftest testing is async]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen :include-macros true]
            [clojure.test.check.properties :as prop :include-macros true]
            [clojure.test.check.results :as results]
            [clojure.test.check.clojure-test :as ct]))

(deftest basic-async-test
  (async done
    (testing "a successful async test"
      (tc/async-quick-check
        100
        (prop/for-all* [gen/s-pos-int]
          (fn [x]
            (fn [trial-done-fn]
              (js/setTimeout #(trial-done-fn (pos? x)) 10))))
        :step-fn (comp
                   (fn [{:keys [step] :as qc-state}]
                     (if (#{:succeeded :shrunk} step)
                       (do
                         (println "finished qc async test!")
                         (done))
                       qc-state))
                   ct/default-step-fn)))))

(deftest normal-async-test
  (async done
    (js/setTimeout
      (fn []
        (println "normal async test")
        (is true)
        (done))
      10)))
