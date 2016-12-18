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
            [clojure.test.check.impl :refer [get-current-time-millis]]))

(defrecord QuickCheckState
  [step num-tests so-far-tests shrink-total-steps result smallest abort?])

(defn- mk-qc-state [options]
  (merge {:num-tests 100
          :so-far-tests 0
          :shrunk {:total-nodes-visited 0
                   :depth 0}
          :abort? false}
         options))

(defn- step-fn->old-qc-behavior
  "Makes step-fn to return the same map as it was being returned by quick-check
  before the introduction of step-fn, for the :succeeded and :shrunk steps. For
  other steps, it returns the quick-check state at that point, unmodified.

  The map has the following keys:
  - for :succeeded step: [:result :num-tests :seed]
  - for :shrunk step: [:result :result-data :seed :failing-size :num-tests :fail :shrunk]"
  [step-fn]
  (fn [qc-state]
    (let [{:keys [step property num-tests so-far-tests seed size shrunk]
           :as qc-state} (step-fn qc-state)]
      (case step
        :succeeded
        {:result true :num-tests so-far-tests :seed seed}

        :shrunk
        (let [{:keys [result args]} (rose/root (:result-map-rose qc-state))]
          {:result (results/passing? result)
           :result-data (results/result-data result)
           :seed seed
           :failing-size size
           :num-tests so-far-tests
           :fail (vec args)
           :shrunk shrunk})

        ;; else
        qc-state))))

(defn- make-rng
  [seed]
  (if seed
    [seed (random/make-random seed)]
    (let [non-nil-seed (get-current-time-millis)]
      [non-nil-seed (random/make-random non-nil-seed)])))

(def ^:private steps
  #{:started :trying :succeeded :failed :shrinking :shrunk :aborted})

(defn- shrink
  [{:keys [result-map-rose] :as qc-state} step-fn]
  (let [shrinks-this-depth (rose/children result-map-rose)]
    (loop [qc-state (assoc qc-state :step :shrinking)
           nodes shrinks-this-depth
           current-smallest (rose/root result-map-rose)]
      (if (empty? nodes)
        (let [shrink-result (:result current-smallest)]
          (-> qc-state
              (assoc :step :shrunk)
              (update :shrunk
                      assoc
                      :result (results/passing? shrink-result)
                      :result-data (results/result-data shrink-result)
                      :smallest (:args current-smallest))
              step-fn))
        (let [;; can't destructure here because that could force
              ;; evaluation of (second nodes)
              head (first nodes)
              tail (rest nodes)
              result (:result (rose/root head))
              args (:args (rose/root head))
              qc-state (-> qc-state
                           (update-in [:shrunk :total-nodes-visited] inc))]
          (if (results/passing? result)
            ;; this node passed the test, so now try testing its right-siblings
            (-> qc-state
                (update :shrunk
                        assoc
                        :args args
                        :result result
                        :pass? true
                        :smallest current-smallest)
                step-fn
                (recur tail current-smallest))
            ;; this node failed the test, so check if it has children,
            ;; if so, traverse down them. If not, save this as the best example
            ;; seen now and then look at the right-siblings
            ;; children
            (let [new-smallest (rose/root head)
                  qc-state (-> qc-state
                               (update :shrunk
                                       assoc
                                       :args args
                                       :result result
                                       :pass? false
                                       :smallest new-smallest))]
              (if-let [children (seq (rose/children head))]
                (recur (step-fn (update-in qc-state [:shrunk :depth] inc)) children new-smallest)
                (recur (step-fn qc-state) tail new-smallest)))))))))

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

  `:step-fn`
    A callback function that will be called at various points in the test
    run with a map (the quick-check state) like:

      ;; called after a trial
      {:step      :trying
       :property  #<...>
       :so-far-tests <number of tests run so far>
       :num-tests <total number of tests>}

      ;; called when a failure is found
      {:step         :failed
       :property     #<...>
       :result       ...
       :so-far-tests <tests ran before failure found>
       :num-tests    <total number of tests>
       :result-map-rose <rose-tree of result maps. root has failing args>}

    It must return the (potentially modified) QC state record

    It will also be called on :started, :succeeded, :shrinking and :shrunk.

    State flow diagram:

           started
              v
      trial, trial, [...]
              v
    succeeded | failure
                     v
        shrinking, shrinking, [...]
                     v
                   shrunk

  Examples:

      (def p (for-all [a gen/pos-int] (> (* a a) a)))

      (quick-check 100 p)
      (quick-check 200 p
                   :seed 42
                   :max-size 50
                   :step-fn (fn [m]
                              (when (= :failed (:step m))
                                (println \"Uh oh...\"))
                              m))"
  [num-tests property & {:keys [seed max-size step-fn]
                         :or {max-size 200, step-fn identity}}]
  (let [[created-seed rng] (make-rng seed)
        ; adapt step-fn for backwards compatibility
        step-fn (step-fn->old-qc-behavior step-fn)]
    (loop [{:keys [num-tests so-far-tests step]
            :as qc-state} (step-fn
                             (mk-qc-state {:num-tests num-tests
                                           :step :started
                                           :seed created-seed
                                           :property property}))
           size-seq (gen/make-size-range-seq max-size)
           rstate rng]
      (if (== so-far-tests num-tests)
        (step-fn (assoc qc-state :step :succeeded))
        (let [[size & rest-size-seq] size-seq
              [r1 r2] (random/split rstate)
              result-map-rose (gen/call-gen property r1 size)
              result (:result (rose/root result-map-rose))
              qc-state (-> qc-state
                            (update :so-far-tests inc)
                            (assoc :size size
                                   :step :trying
                                   :result-map-rose result-map-rose
                                   :result result)
                            step-fn)]
          (if (results/passing? result)
            (recur qc-state rest-size-seq r2)
            (-> qc-state
                (assoc :step :failed)
                step-fn
                (shrink step-fn))))))))
