;   Copyright (c) Rich Hickey, Reid Draper, and contributors.
;   All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.test.check
  (:require [clojure.test.check.generators :as gen]
            [clojure.test.check.random :as random]
            [clojure.test.check.results :as results]
            [clojure.test.check.rose-tree :as rose]
            [clojure.test.check.impl :refer [get-current-time-millis
                                             exception-like?]]))

(declare shrink-loop failure)

(defn- make-rng
  [seed]
  (if seed
    [seed (random/make-random seed)]
    (let [non-nil-seed (get-current-time-millis)]
      [non-nil-seed (random/make-random non-nil-seed)])))

(defn- complete
  [property num-trials seed reporter-fn]
  (reporter-fn {:type :complete
                :property property
                :result true
                :num-tests num-trials
                :seed seed})

  {:result true :num-tests num-trials :seed seed})

(defn quick-check-step
  [{:keys [type] :as qc-state}]
  (case type
    nil  ;; initial state
    (let [{:keys [seed max-size] :or {max-size 200}} qc-state
          [created-seed rng] (make-rng seed)
          size-seq (gen/make-size-range-seq max-size)]
      (recur (assoc qc-state
                    :type :trial
                    :rstate rng
                    :created-seed created-seed
                    :size-seq size-seq)))

    :trial
    (let [{:keys [size-seq rstate property]} qc-state
          [size & rest-size-seq] size-seq
          [r1 r2] (random/split rstate)
          result-map-rose (gen/call-gen property r1 size)
          result (:result (rose/root result-map-rose))
          qc-state (assoc qc-state
                          :result-map-rose result-map-rose
                          :size-seq rest-size-seq
                          :size size)]
      (if (results/passing? result)
        (assoc qc-state :rstate r2)
        (assoc qc-state :type :failure)))

    :failure
    (let [result-map-rose (:result-map-rose qc-state)
          current-smallest (rose/root result-map-rose)
          shrink-nodes (rose/children result-map-rose)]
      (recur (assoc qc-state
                    :type :shrink-step
                    :depth 0
                    :current-smallest current-smallest
                    :shrink-nodes shrink-nodes)))

    :shrink-step
    (let [{:keys [result-map-rose shrink-nodes depth]} qc-state]
      (if (empty? shrink-nodes)
        (assoc qc-state :type :shrunk)
        (let [;; can't destructure here because that could force
              ;; evaluation of (second nodes)
              head (first shrink-nodes)
              tail (rest shrink-nodes)
              qc-state (assoc qc-state :current-shrink-node (rose/root head))
              result (:result (rose/root head))]
          (if (results/passing? result)
            ;; this node passed the test, so now try testing its right-siblings
            (assoc qc-state :shrink-nodes tail)
            ;; this node failed the test, so check if it has children,
            ;; if so, traverse down them. If not, save this as the best example
            ;; seen now and then look at the right-siblings children
            (let [new-smallest (rose/root head)]
              (if-let [children (seq (rose/children head))]
                (assoc qc-state
                       :shrink-nodes children
                       :current-smallest new-smallest
                       :depth (inc depth))
                (assoc qc-state
                       :shrink-nodes tail
                       :current-smallest new-smallest)))))))))

(defn qc-state->report-data [qc-state]
  (case (:type qc-state)
    :trial
    (let [{:keys [property result-map-rose created-seed]} qc-state
          {:keys [args result]} (rose/root result-map-rose)]
      {:type        :trial
       :args        args
       :property    property
       :result      result
       :result-data (results/result-data result)
       :seed        created-seed})

    :failure
    (let [{:keys [property type result-map-rose size created-seed]} qc-state
          root (rose/root result-map-rose)
          result (:result root)]
      {:fail         (:args root)
       :type         :failure
       :failing-size size
       :property     property
       :result       (results/passing? result)
       :result-data  (results/result-data result)
       :seed         created-seed})

    :shrink-step
    (let [{:keys [property current-shrink-node current-smallest depth]} qc-state
          {:keys [args result]} current-shrink-node]
      {:type :shrink-step
       :shrinking {:args                args
                   :depth               depth
                   :result              result
                   :result-data         (results/result-data result)
                   :smallest            (:args current-smallest)}})

    :shrunk
    (let [{:keys [current-smallest depth]} qc-state
          {:keys [result args]} current-smallest]
      {:depth depth
       :result (results/passing? result)
       :result-data (results/result-data result)
       :smallest args})))

(defn quick-check
  "Tests `property` `num-tests` times.

  Takes several optional keys:

  `:seed`
    Can be used to re-run previous tests, as the seed used is returned
    after a test is run.

  `:max-size`.
    can be used to control the 'size' of generated values. The size will
    start at 0, and grow up to max-size, as the number of tests increases.
    Generators will use the size parameter to bound their growth. This
    prevents, for example, generating a five-thousand element vector on
    the very first test.

  `:reporter-fn`
    A callback function that will be called at various points in the test
    run, with a map like:

      ;; called after a passing trial
      {:type            :trial
       :args            [...]
       :num-tests       <number of tests run so far>
       :num-tests-total <total number of tests to be run>
       :seed            42
       :property        #<...>
       :result          true
       :result-data     {...}}

      ;; called after the first failing trial
      {:type         :failure
       :fail         [...failing args...]
       :failing-size 13
       :num-tests    <tests ran before failure found>
       :property     #<...>
       :result       false/exception
       :result-data  {...}
       :seed         42}

    It will also be called on :complete, :shrink-step and :shrunk.

  Examples:

      (def p (for-all [a gen/pos-int] (> (* a a) a)))

      (quick-check 100 p)
      (quick-check 200 p
                   :seed 42
                   :max-size 50
                   :reporter-fn (fn [m]
                                  (when (= :failure (:type m))
                                    (println \"Uh oh...\"))))"
  [num-tests property & {:keys [seed max-size reporter-fn]
                         :or {max-size 200, reporter-fn (constantly nil)}}]
  (loop [so-far 0
         qc-state {:property property
                   :max-size max-size
                   :seed seed}]
    (if (== so-far num-tests)
      (complete property num-tests (:created-seed qc-state) reporter-fn)
      (let [qc-state (quick-check-step qc-state)
            so-far (inc so-far)]
        (case (:type qc-state)
          :trial
          (do
            (reporter-fn (-> (qc-state->report-data qc-state)
                             (assoc :num-tests so-far
                                    :num-tests-total num-tests)) )
            (recur so-far qc-state))

          :failure
          (failure qc-state so-far reporter-fn))))))

(defn- shrink-loop
  "Shrinking a value produces a sequence of smaller values of the same type.
  Each of these values can then be shrunk. Think of this as a tree. We do a
  modified depth-first search of the tree:
  Do a non-exhaustive search for a deeper (than the root) failing example.
  Additional rules added to depth-first search:
  * If a node passes the property, you may continue searching at this depth,
  but not backtrack
  * If a node fails the property, search its children
  The value returned is the left-most failing example at the depth where a
  passing example was found.
  Calls reporter-fn on every shrink step."
  [qc-state reporter-fn]
  (loop [qc-state (quick-check-step qc-state)
         total-nodes-visited 0]
    (case (:type qc-state)
      :shrunk
      (-> (qc-state->report-data qc-state)
          (assoc :total-nodes-visited total-nodes-visited))

      :shrink-step
      (do
        (reporter-fn (-> (qc-state->report-data qc-state)
                         (assoc-in [:shrinking :total-nodes-visited] total-nodes-visited)))
        (recur (quick-check-step qc-state) (inc total-nodes-visited))))))

(defn- failure [qc-state trial-number reporter-fn]
  (let [failure-data (-> (qc-state->report-data qc-state)
                         (assoc :num-tests trial-number))]

    (reporter-fn (assoc failure-data :type :failure))

    (let [shrunk (shrink-loop qc-state #(reporter-fn (merge failure-data %)))]
      (reporter-fn (assoc failure-data
                          :type :shrunk
                          :shrunk shrunk))
      (-> failure-data
          (dissoc :property)
          (assoc :shrunk shrunk)))))
