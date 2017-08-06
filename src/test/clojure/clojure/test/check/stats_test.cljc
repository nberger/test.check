;   Copyright (c) Rich Hickey, Reid Draper, and contributors.
;   All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.test.check.stats-test
  (:require #?(:clj  [clojure.test :as test :refer [deftest testing is]]
               :cljs [cljs.test :as test :refer-macros [deftest testing is]])
            [clojure.string :as string]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.stats :as stats]
            [clojure.test.check.clojure-test :as t.c.ct]
            [clojure.test.check.properties :as prop #?@(:cljs [:include-macros true])]))

(deftest labels-all-test-cases
  (testing "a prop that fails"
    (let [p (-> (prop/for-all [i gen/s-pos-int] (< i 50))
                (stats/classify (partial > 10) :lt-10)
                (stats/classify (partial <= 10) :gte-10))
          result-map (tc/quick-check 100 p
                                     :step-fn stats/conj-labels-step-fn)
          ok-tests (:so-far-tests result-map)
          labels-count (->> result-map :labels (apply concat) count)]
      (is (= ok-tests labels-count))
      (is (= #{:lt-10 :gte-10} (->> result-map :labels (apply concat) set)))))

  (testing "a prop that doesn't fail"
    (let [p (-> (prop/for-all [i gen/s-pos-int] (< 0 i))
                (stats/classify (partial > 10) :lt-10)
                (stats/classify (partial <= 10) :gte-10))
          result-map (tc/quick-check 100 p
                                     :step-fn stats/conj-labels-step-fn)
          ok-tests (:so-far-tests result-map)
          labels-count (->> result-map :labels (apply concat) count)]
      (is (= ok-tests labels-count))
      (is (= #{:lt-10 :gte-10} (->> result-map :labels (apply concat) set))))))

(deftest for-all-classify-test
  (let [p (-> (prop/for-all [i gen/pos-int] (< i 50))
              (stats/classify (fn [x] (> 10 x)) :lt-10)
              (stats/classify (fn [x] (> 20 x)) :lt-20)
              (stats/classify (fn [x] (> 30 x)) :lt-30)
              (stats/classify (fn [x] (<= 30 x)) :gte-30))
        result-map (tc/quick-check 100 p
                                   :step-fn stats/conj-labels-step-fn)
        num-tests (:num-tests result-map)
        percentages (->> (:labels result-map)
                         (apply concat)
                         frequencies
                         (map (fn [[k v]] [k (/ v num-tests)]))
                         (into {}))]
    (is (> (percentages :lt-10) 0.1) "some values under 10")
    (is (> (percentages :lt-20) 0.2) "some values under 20")
    (is (> (percentages :lt-30) 0.4) "some values under 30")
    (is (= #{:lt-10 :lt-20 :lt-30 :gte-30} (-> percentages keys set)) "all the labels are present")
    (is (< 1 (->> percentages vals (apply +))) "every trial generated at least a label")))

(deftest for-all-collect-test
  (testing "collect args"
    (let [p (-> (prop/for-all [i (gen/choose 1 3)] (< i 50))
                stats/collect)
          result-map (tc/quick-check 100 p
                                     :step-fn stats/conj-labels-step-fn)
          num-tests (:num-tests result-map)
          percentages (->> (:labels result-map)
                           (apply concat)
                           frequencies
                           (map (fn [[k v]] [k (/ v num-tests)]))
                           (into {}))]
      (is (> (percentages [1]) 0.01))
      (is (> (percentages [2]) 0.01))
      (is (> (percentages [3]) 0.01))
      (is (= #{[1] [2] [3]} (-> percentages keys set)) "all the labels are present")
      (is (= 1 (->> percentages vals (apply +))) "every trial generated at least a label")))

  (testing "collect count of gen'ed vector"
    (let [p (-> (prop/for-all [[i & _] (gen/vector gen/s-pos-int 1 3)]
                  (> i 0))
                (stats/collect count))
          result-map (tc/quick-check 100 p
                                     :step-fn stats/conj-labels-step-fn)
          num-tests (:num-tests result-map)
          labels (->> result-map :labels (apply concat))
          percentages (->> labels
                           frequencies
                           (map (fn [[k v]] [k (/ v num-tests)]))
                           (into {}))]
      (is (> (percentages 1) 0.01))
      (is (> (percentages 2) 0.01))
      (is (> (percentages 3) 0.01))
      (is (= #{1 2 3} (-> percentages keys set)) "all the labels are present")
      (is (= 1 (->> percentages vals (apply +))) "every trial generated exactly 1 label"))))

(deftest print-stats-test
  (let [labels [[:lt-10]
                [:lt-10 :lt-20]
                [:lt-10]
                nil]
        out (with-out-str (stats/print 5 labels))
        lines (string/split-lines out)]
    (is (= 2 (count lines)))
    (is (= "40.0% :lt-10" (first lines)))
    (is (= "20.0% :lt-10, :lt-20" (second lines)))))

(deftest for-all-classify-test-print-stats
  (testing "prints stats after a test that fails"
    (let [p (-> (prop/for-all [i gen/pos-int] (< i 50))
                (stats/classify (fn [x] (< x 0)) :negative)
                (stats/classify (fn [x] (< x 10)) :lt-10)
                (stats/classify (fn [x] (< x 20)) :lt-20)
                (stats/classify (fn [x] (< x 30)) :lt-30)
                (stats/classify (fn [x] (>= x 30)) :gte-30))
          test-out (with-out-str
                     (tc/quick-check 100 p
                                     :step-fn (comp stats/print-stats-step-fn stats/conj-labels-step-fn)))
          lines (string/split-lines test-out)]
      (is (= 4 (count lines))
          "prints out one line for each label line")
      (is (some #(re-find #"^\d+\.\d% :lt-10, :lt-20, :lt-30$" %) lines))
      (is (some #(re-find #"^\d+\.\d% :lt-20, :lt-30$" %) lines))
      (is (some #(re-find #"^\d+\.\d% :lt-30$" %) lines))
      (is (some #(re-find #"^\d+\.\d% :gte-30$" %) lines))))

  (testing "prints stats after a test that succeeds"
    (let [p (-> (prop/for-all [i gen/s-pos-int] (pos? i))
                (stats/classify (fn [x] (< x 0)) :negative)
                (stats/classify (fn [x] (< x 10)) :lt-10)
                (stats/classify (fn [x] (< x 20)) :lt-20)
                (stats/classify (fn [x] (< x 30)) :lt-30)
                (stats/classify (fn [x] (>= x 30)) :gte-30))
          test-out (with-out-str
                     (tc/quick-check 100 p
                                     :step-fn (comp stats/print-stats-step-fn stats/conj-labels-step-fn)))
          lines (string/split-lines test-out)]
      (is (= 4 (count lines))
          "prints out one line for each label line")
      (is (some #(re-find #"^\d+\.\d% :lt-10, :lt-20, :lt-30$" %) lines))
      (is (some #(re-find #"^\d+\.\d% :lt-20, :lt-30$" %) lines))
      (is (some #(re-find #"^\d+\.\d% :lt-30$" %) lines))
      (is (some #(re-find #"^\d+\.\d% :gte-30$" %) lines)))))
