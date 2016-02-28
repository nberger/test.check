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
          result-map (tc/quick-check 100 p)
          ok-tests (:num-tests result-map)
          labels-count (->> result-map :labels (apply concat) count)]
      (is (= ok-tests labels-count))
      (is (= #{:lt-10 :gte-10} (->> result-map :labels (apply concat) set)))))

  (testing "a prop that doesn't fail"
    (let [p (-> (prop/for-all [i gen/s-pos-int] (< 0 i))
                (stats/classify (partial > 10) :lt-10)
                (stats/classify (partial <= 10) :gte-10))
          result-map (tc/quick-check 100 p)
          ok-tests (:num-tests result-map)
          labels-count (->> result-map :labels (apply concat) count)]
      (is (= ok-tests labels-count))
      (is (= #{:lt-10 :gte-10} (->> result-map :labels (apply concat) set))))))

(deftest for-all-classify-test
  (let [p (-> (prop/for-all [i gen/pos-int] (< i 50))
              (stats/classify (fn [x] (> 10 x)) :lt-10)
              (stats/classify (fn [x] (> 20 x)) :lt-20)
              (stats/classify (fn [x] (> 30 x)) :lt-30)
              (stats/classify (fn [x] (<= 30 x)) :gte-30))
        result-map (tc/quick-check 100 p)
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
          result-map (tc/quick-check 100 p)
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
          result-map (tc/quick-check 100 p)
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
                [:lt-10]
                [:lt-10 :lt-20]
                nil]
        out (with-out-str (stats/print 5 labels))
        lines (string/split-lines out)]
    (is (= 3 (count lines)))
    (is (some #{"40.0% :lt-10"} lines))
    (is (some #{"20.0% :lt-10, :lt-20"} lines))
    (is (some #{"20.0% No labels"} lines))))

(defmacro with-test-out-str
  "Evaluates exprs in a context in which *test-out* is bound to a fresh
  StringWriter.  Returns the string created by any nested printing
  calls.
  Modified version of clojure.core/with-out-str"
  [& body]
  `(let [s# (new java.io.StringWriter)]
     (binding [test/*test-out* s#]
       ~@body
       (str s#))))

(deftest for-all-classify-test-print-stats
  (binding [clojure.test.check.clojure-test/*report-stats* true]
    (let [p (-> (prop/for-all [i gen/pos-int] (< i 50))
                (stats/classify (fn [x] (< x 10)) :lt-10)
                (stats/classify (fn [x] (< x 20)) :lt-20)
                (stats/classify (fn [x] (< x 30)) :lt-30)
                (stats/classify (fn [x] (>= x 30)) :gte-30))
          test-out (with-test-out-str
                     (tc/quick-check 100 p
                                     :reporter-fn t.c.ct/default-reporter-fn))
          lines (string/split-lines test-out)]
      (is (= 5 (count lines))
          "prints out one line for each label line + one line for the ct/report :type :shrunk")
      (is (some #(re-find #"^\{:type :clojure.test.check.clojure-test/shrunk" %) lines)
          "prints one line for the ct/report :type :shrunk")
      (is (some #(re-find #"^\d+\.\d% :lt-10, :lt-20, :lt-30$" %) lines))
      (is (some #(re-find #"^\d+\.\d% :lt-20, :lt-30$" %) lines))
      (is (some #(re-find #"^\d+\.\d% :lt-30$" %) lines))
      (is (some #(re-find #"^\d+\.\d% :gte-30$" %) lines)))))
