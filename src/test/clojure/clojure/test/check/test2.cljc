(ns clojure.test.check.test2
  (:require
    #?(:cljs [cljs.test :as test :refer-macros [deftest testing is]]
       :clj  [clojure.test :refer :all])
    [clojure.test.check2 :as tc2]
    [clojure.test.check.generators :as gen #?@(:cljs [:include-macros true])]
    [clojure.test.check.properties :as prop #?@(:cljs [:include-macros true])]))

(deftest step-fn-calls-test
  (testing "a failing prop"
    (let [calls (atom [])
          step-fn (fn [qc-state]
                    (swap! calls conj qc-state)
                    qc-state)
          prop (prop/for-all [n gen/nat]
                 (> 5 n))]
      (tc2/quick-check 1000 prop {:step-fn step-fn})
      (is (= #{:started :trying :failed :shrinking :shrunk}
             (->> @calls (map :step) set)))))

  (testing "a successful prop"
    (let [calls (atom [])
          step-fn (fn [qc-state]
                    (swap! calls conj qc-state)
                    qc-state)
          prop (prop/for-all [n gen/nat]
                 (<= 0 n))]
      (tc2/quick-check 5 prop {:step-fn step-fn})
      (is (= #{:started :trying :succeeded}
             (->> @calls (map :step) set))))))

(deftest shrinking-step-events-test
  (let [events (atom [])
        step-fn (fn [qc-state]
                  (swap! events conj qc-state)
                  qc-state)
        pred (fn [n] (not (< 100 n)))
        prop (prop/for-all [n (gen/scale (partial * 10) gen/nat)]
               (pred n))]
    (tc2/quick-check 100 prop {:step-fn step-fn})
    (let [shrink-steps (filter #(= :shrinking (:step %)) @events)
          pass? (comp :pass? :shrunk)
          failing-steps (filter (complement pass?) shrink-steps)
          passing-steps (filter pass? shrink-steps)
          get-shrunk-args (comp first :args :shrunk)
          get-smallest-args (comp first :args :smallest :shrunk)
          get-args-and-smallest-args (juxt get-shrunk-args get-smallest-args)]
      (is (seq failing-steps))
      (is (every? #(not (pred (get-shrunk-args %))) failing-steps)
          "pred on args is falsey in all failing steps")
      (is (every? #(pred (get-shrunk-args %)) passing-steps)
          "pred on args is truthy in all passing steps")
      (is (->> failing-steps
               (map get-args-and-smallest-args)
               (every? (fn [[args current-smallest-args]]
                         (= args current-smallest-args))))
          "for every failing step, current-smallest args are equal to args")
      (is (->> passing-steps
               (map get-args-and-smallest-args)
               (every? (fn [[args current-smallest-args]] (< args current-smallest-args))))
          "for every passing step, current-smallest args are smaller than args")
      (let [shrunk-args (map get-shrunk-args failing-steps)]
        (is (= shrunk-args
               (reverse (sort shrunk-args)))
            "failing steps args are sorted in descending order")))))
