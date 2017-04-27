;   Copyright (c) Rich Hickey, Reid Draper, and contributors.
;   All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.test.check.async-test
  (:require [cljs.test :as test :refer-macros [deftest is async]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen :include-macros true]
            [clojure.test.check.properties :as prop :include-macros true]
            [clojure.test.check.clojure-test :as ct :include-macros true]))

(deftest basic-successful-async-test
  (async done
    (tc/async-quick-check
      100
      (prop/for-all* [gen/s-pos-int]
        (fn [x]
          (fn [trial-done-fn]
            (js/setTimeout #(trial-done-fn (pos? x)) 10))))
      :step-fn (fn [{:keys [step] :as qc-state}]
                 (if (= :succeeded step)
                   (done)
                   qc-state)))))

(deftest basic-failing-async-quick-check
  (async done
    (tc/async-quick-check
      100
      (prop/for-all* [(gen/vector gen/s-pos-int)]
        (fn [coll]
          (fn [trial-done-fn]
            (js/setTimeout #(trial-done-fn (every? (partial > 10) coll)) 10))))
      :step-fn (fn [{:keys [step] :as qc-state}]
                 (if (= :shrunk step)
                   (do
                     ;; the chance of this assertion to fail must be really low
                     (is (= [[10]] (-> qc-state :shrunk :smallest)))
                     (done))
                   qc-state)))))

(ct/defspec-async basic-successful-defspec-async-test
  (prop/for-all* [gen/s-pos-int]
                 (fn [x]
                   (fn [trial-done-fn]
                     (js/setTimeout #(trial-done-fn (pos? x))
                                    10)))))

;; the failing example is commented out because our test suite would fail
#_(ct/defspec-async basic-successful-defspec-async-test
  (prop/for-all* [[gen/s-pos-int]]
                 (fn [coll]
                   (fn [trial-done-fn]
                     (js/setTimeout #(trial-done-fn (every? (partial > 10) coll))
                                    10)))))
